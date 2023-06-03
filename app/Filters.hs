module Filters (
        opts,
        Data,
        filterRounds,
    ) where

import Options.Applicative

import Salmon

import qualified Data.Map as M

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Data.Text (Text)

import Data.Time
    ( fromGregorian,
      toGregorian,
      UTCTime(..),
      TimeLocale,
      TimeZone,
      parseTimeM,
      localTimeToUTC ) 
import Data.Foldable1 (Foldable1(foldrMap1))

data Pred = Player Text
          | Stage Text
          | TimeBefore UTCTime
          | TimeAfter UTCTime
        deriving (Read, Show, Eq, Ord)

data Join = U -- Union
          | I -- Intersection
        deriving (Read, Show, Eq, Ord, Bounded, Enum)


data Filter = Filter Join Pred
        deriving (Read, Show, Eq, Ord)

type Data = [Filter]

opts :: UTCTime -> TimeZone -> TimeLocale -> Parser [Filter]
opts UTCTime{utctDay=day} zone local = 
    -- this allows you to repeat multiple before and afters, which is not ideal
    -- but better than nothing
    many $ asum
    [ (option (Filter I . TimeBefore <$> maybeReader (parseTime))
            (long "before" <> short 'b' <> metavar timeFMT <> help "Filter matches after this time"))
    , (option (Filter I . TimeAfter <$> maybeReader (parseTime))
            (long "after" <> short 'a' <> metavar timeFMT <> help "Filter matches before this time"))
    , (Filter U . Player 
            <$> strOption 
                (long "player" <> short 'p' <> metavar "PLAYER" <> help "Filter matches to ones with PLAYER"))
    , (Filter U . Stage <$> strOption
                (long "stage" <> short 's' <> metavar "STAGE" <> help "Filter matches to ones on STAGE"))
    ]
   where timeFMT = "[yyyy-]mm-ddThh:mm[:ss][.sss]"
         -- so many time formats...
         parseTime timeStr = 
            let (startYear, _, _) = toGregorian day
                in asum -- whichever one passes
            -- assume current day if not given a day (replace offset (utctDayTime) only)
            [ UTCTime day . utctDayTime . localTimeToUTC zone <$> parseTimeM False local "%H" timeStr
            , UTCTime day . utctDayTime . localTimeToUTC zone <$> parseTimeM False local "%R" timeStr
            , UTCTime day . utctDayTime . localTimeToUTC zone <$> parseTimeM False local "%T" timeStr
            -- assume current year if not given a year
            , replaceYear startYear . localTimeToUTC zone <$> parseTimeM True local "%m-%dT%T" timeStr
            , replaceYear startYear . localTimeToUTC zone <$> parseTimeM True local "%m-%d_%T" timeStr
            , replaceYear startYear . localTimeToUTC zone <$> parseTimeM True local "%m-%dT%R" timeStr
            , replaceYear startYear . localTimeToUTC zone <$> parseTimeM True local "%m-%d_%R" timeStr
            , replaceYear startYear . localTimeToUTC zone <$> parseTimeM True local "%m-%dT%H" timeStr
            , replaceYear startYear . localTimeToUTC zone <$> parseTimeM True local "%m-%d_%H" timeStr
            , replaceYear startYear . localTimeToUTC zone <$> parseTimeM True local "%m-%d" timeStr
            , replaceYear startYear . localTimeToUTC zone <$> parseTimeM True local "%m-%d" timeStr
            -- if given everything, just convert
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%dT%T" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d_%T" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%dT%R" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d_%R" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%dT%H" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d_%H" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d" timeStr
            ]
        -- clips invalid values
         replaceYear year (UTCTime d t) = let (_, curMonth, curDay) = toGregorian d
                                  in UTCTime (fromGregorian year curMonth curDay) t


-- convert a Predicate to its implementation
fromPred  :: Pred -> (Round -> Bool)
fromPred = \cases
    (Player name)  -> (isTeammate name)
    (Stage  name)  -> ((== name) . stage)
    (TimeBefore t) -> ((< t) . time)
    (TimeAfter t)  -> ((> t) . time)


-- folds a list of filters into a single filter instruction
fromFilters :: NonEmpty Filter -> (Round -> Bool)
fromFilters  l = foldrMap1 firstFilter foldFilter l
    where -- all other filters hold and this one does too
          foldFilter (Filter I p) f = (fromPred p `andF` f)
          -- either P is true and all previous filters hold or P is false
          -- and all previous filters hold
          foldFilter (Filter U p) f = ((fromPred p `andF` f) `orF` f)

          -- first filter must always hold, regardless of Union or Intersection
          firstFilter (Filter _ p) = (\r -> fromPred p $ r)

          andF f1 f2 = (\r -> f1 r && f2 r)
          orF  f1 f2 = (\r -> f1 r || f2 r)

-- filters the roundMap given the list of filters to apply
-- assumes that the empty list means no filter
filterRounds :: [Filter] -> RoundMap -> RoundMap
filterRounds [] = id
filterRounds l  = M.filter . fromFilters . NE.fromList $ l -- safe due to earlier case for []

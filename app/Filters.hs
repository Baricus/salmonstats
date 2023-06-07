module Filters (
        opts,
        Data,
        filterRounds,
    ) where

import Options.Applicative

import Salmon

import qualified Data.Map as M

import Data.Text (Text)

import Data.Time
    ( fromGregorian,
      toGregorian,
      UTCTime(..),
      TimeLocale,
      TimeZone,
      parseTimeM,
      localTimeToUTC ) 

data Pred = Player Text
          | Stage Text
          | TimeBefore UTCTime
          | TimeAfter UTCTime
          | Any  -- always true
        deriving (Read, Show, Eq, Ord)

-- generic in the filter to allow for transformations!
data Filter op = And (Filter op) (Filter op)
               | Or (Filter op) (Filter op)
               | P op
        deriving (Read, Show, Eq, Ord)

instance Functor Filter where
    fmap :: (a -> b) -> Filter a -> Filter b
    fmap f = \cases
        (P v)     -> P $ f v
        (And l r) -> And (fmap f l) (fmap f r)
        (Or  l r) -> Or (fmap f l) (fmap f r)

type Data = Filter Pred

opts :: UTCTime -> TimeZone -> TimeLocale -> Parser (Filter Pred)
opts UTCTime{utctDay=day} zone local = 
    -- this allows you to repeat multiple before and afters, which is not ideal
    -- but better than nothing
    buildAndfilter <$> many (asum
    [ (option (TimeBefore <$> maybeReader (parseTime))
            (long "before" <> short 'b' <> metavar timeFMT <> help "Filter matches after this time"))
    , (option (TimeAfter <$> maybeReader (parseTime))
            (long "after" <> short 'a' <> metavar timeFMT <> help "Filter matches before this time"))
    , (Player 
            <$> strOption 
                (long "player" <> short 'p' <> metavar "PLAYER" <> help "Filter matches to ones with PLAYER"))
    , (Stage <$> strOption
                (long "stage" <> short 's' <> metavar "STAGE" <> help "Filter matches to ones on STAGE"))
    ])
   where timeFMT = "[[yyyy-]mm-dd]_hh[:mm[[:ss]]]"
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
            -- if given full day and partial/no time, replace missing info with 00:00:00 as needed
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%dT%R" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d_%R" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%dT%H" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d_%H" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d" timeStr
            -- if given everything, just convert
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%dT%T" timeStr
            , localTimeToUTC zone <$> parseTimeM False local "%Y-%m-%d_%T" timeStr
            ]
        -- clips invalid values
         replaceYear year (UTCTime d t) = let (_, curMonth, curDay) = toGregorian d
                                  in UTCTime (fromGregorian year curMonth curDay) t

-- takes a list of predicates and adds them
-- into a single "anded" together clause
buildAndfilter :: [Pred] -> Filter Pred
buildAndfilter = foldr (\p f -> And (P p) f) (P Any)

-- convert a Predicate to its implementation
fromPred  :: Pred -> (Round -> Bool)
fromPred = \cases
    (Player name)  -> (isTeammate name)
    (Stage  name)  -> ((== name) . stage)
    (TimeBefore t) -> ((< t) . time)
    (TimeAfter t)  -> ((> t) . time)
    (Any)          -> const True

-- collapses a boolean filter to a single function
fromFilters :: Filter (Round -> Bool) -> (Round -> Bool)
fromFilters = \cases
    (P p)     -> p
    (And l r) -> liftA2 (&&) (fromFilters l) (fromFilters r)
    (Or  l r) -> liftA2 (||) (fromFilters l) (fromFilters r)

-- filters the roundMap given the list of filters to apply
-- assumes that the empty list means no filter
filterRounds :: Filter Pred -> RoundMap -> RoundMap
filterRounds = M.filter . fromFilters . fmap fromPred

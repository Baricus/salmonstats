module Filters (
        opts,
    ) where

import Options.Applicative

import Salmon

import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Set as S
import Data.Maybe (catMaybes)

import Data.Time
    ( fromGregorian,
      toGregorian,
      UTCTime(..),
      TimeLocale,
      TimeZone,
      parseTimeM,
      localTimeToUTC ) 

 -- Predicates that we can filter on
data Pred = PlayerName Text
          | Stage Text
          | TimeBefore UTCTime
          | TimeAfter UTCTime
          | FilterPrivateLobbies
          | Any  -- always true
          | Negate Pred
        deriving (Read, Show, Eq, Ord)

-- Things that affect results that aren't predicates on individual filters
-- but instead of the set of games as a whole
newtype Modifiers = Last Int

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

type Data = (Filter Pred, [Modifiers])

opts :: UTCTime -> TimeZone -> TimeLocale -> Parser (RoundMap -> RoundMap)
opts UTCTime{utctDay=day} zone local = 
    filterRounds <$> liftA2 (,)
        -- first the filters
        (buildAndfilter <$>
            -- we combine the two kinds of options with <>
            liftA2 (<>)
                -- We can only have one of these options
                (fmap catMaybes . traverse optional $
                    [ flag FilterPrivateLobbies Any 
                           (long "include-private" <> help "Include private battles in computed statistics")
                    , option (TimeBefore <$> maybeReader parseTime)
                           (long "before" <> short 'b' <> metavar timeFMT <> help "Filter matches after this time")
                    , option (TimeAfter <$> maybeReader parseTime)
                           (long "after" <> short 'a' <> metavar timeFMT <> help "Filter matches before this time")
                    ]
                )
                -- we can have many of these options
                (many . asum $
                    [ PlayerName <$> strOption 
                               (long "player" <> short 'p' <> metavar "PLAYER" <> help "Filter matches to ones with PLAYER as a teammate; can be ID or any username used")
                    , Negate . PlayerName <$> strOption
                               (long "not-player" <> metavar "PLAYER" <> help "Filter matches to ones without PLAYER as a teammate")
                    , Stage <$> strOption
                               (long "stage" <> short 's' <> metavar "STAGE" <> help "Filter matches to ones on STAGE")
                    ]
                ))
        -- then the modifiers
        ( fmap catMaybes . traverse optional $
            [ option (Last <$> auto)
                 (long "last" <> short 'l' <> metavar "MATCHES" <> help "Filter for the last MATCHES matches played")
            ]
        )
   where timeFMT = "[[yyyy-]mm-dd]_hh[:mm[[:ss]]]"
         -- so many time formats in timeFMT...
         parseTime timeStr = 
            let (startYear, _, _) = toGregorian day
                in asum -- whichever one passes
            -- assume current day if not given a day (replace offset (utctDayTime) only)
            [ UTCTime day . utctDayTime . localTimeToUTC zone <$> parseTimeM True local "%H" timeStr
            , UTCTime day . utctDayTime . localTimeToUTC zone <$> parseTimeM True local "%R" timeStr
            , UTCTime day . utctDayTime . localTimeToUTC zone <$> parseTimeM True local "%T" timeStr
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
            , localTimeToUTC zone <$> parseTimeM True local "%Y-%m-%dT%R" timeStr
            , localTimeToUTC zone <$> parseTimeM True local "%Y-%m-%d_%R" timeStr
            , localTimeToUTC zone <$> parseTimeM True local "%Y-%m-%dT%H" timeStr
            , localTimeToUTC zone <$> parseTimeM True local "%Y-%m-%d_%H" timeStr
            , localTimeToUTC zone <$> parseTimeM True local "%Y-%m-%d" timeStr
            , localTimeToUTC zone <$> parseTimeM True local "%Y-%m-%d" timeStr
            -- if given everything, just convert
            , localTimeToUTC zone <$> parseTimeM True local "%Y-%m-%dT%T" timeStr
            , localTimeToUTC zone <$> parseTimeM True local "%Y-%m-%d_%T" timeStr
            ]
        -- clips invalid values
         replaceYear year (UTCTime d t) = let (_, curMonth, curDay) = toGregorian d
                                  in UTCTime (fromGregorian year curMonth curDay) t

-- takes a list of predicates and adds them
-- into a single "anded" together clause
buildAndfilter :: [Pred] -> Filter Pred
buildAndfilter = foldr (And . P) (P Any)

-- convert a Predicate to its implementation
-- Requires a list of gameIDs to for things like Last
fromPred  :: Pred -> (Round -> Bool)
fromPred = \cases
    (PlayerName name)          -> (isTeammate name)
    (Stage  name)          -> ((== name) . stage)
    (TimeBefore t)         -> ((< t) . time)
    (TimeAfter t)          -> ((> t) . time)
    (FilterPrivateLobbies) -> maybe True
                                        (\cases
                                            (PrivateScenario _) -> False
                                            _                   -> True)
                                . shift
    Any                    -> const True
    (Negate p)             -> not <$> fromPred p

-- collapses a boolean filter to a single function
fromFilters :: Filter (Round -> Bool) -> (Round -> Bool)
fromFilters = \cases
    (P p)     -> p
    (And l r) -> liftA2 (&&) (fromFilters l) (fromFilters r)
    (Or  l r) -> liftA2 (||) (fromFilters l) (fromFilters r)

-- runs modifiers on the roundMap
runModifiers :: [Modifiers] -> RoundMap -> RoundMap
runModifiers [] rMap = rMap
runModifiers (m : ms) rMap =
    case m of
         Last n -> let ids = getIDs rMap in runModifiers ms 
                    $ M.filter (flip S.member (S.fromList (take n ids)) . gameID) rMap


-- filters the roundMap given the list of filters to apply
-- assumes that the empty list means no filter
filterRounds :: Data -> RoundMap -> RoundMap
filterRounds (filt, mods) rMap = runModifiers mods $ M.filter (fromFilters . fmap fromPred $ filt) rMap

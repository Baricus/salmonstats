module Filters where

import Options.Applicative

import Salmon

import Salmon.BossMap (BossMap)
import qualified Salmon.BossMap as BM

import Data.Text (Text)

import Data.Maybe (maybeToList)

import Data.Time 
import Data.Time.Format
import Data.Time.Calendar.OrdinalDate


data Filter = Player Text
            | TimeBefore UTCTime
            | TimeAfter UTCTime
            | Stage Text
        deriving (Read, Show, Eq, Ord)

type Data = [Filter]

opts :: UTCTime -> TimeZone -> TimeLocale -> Parser [Filter]
opts UTCTime{utctDay=day} zone local = 
    asum 
    [ maybeToList <$> optional
        (option (TimeBefore <$> maybeReader (parseTime))
            (long "before" <> short 'b' <> metavar timeFMT <> help "Filter matches after this time"))
    , maybeToList <$> optional
        (option (TimeAfter <$> maybeReader (parseTime))
            (long "after" <> short 'a' <> metavar timeFMT <> help "Filter matches before this time"))
    , many 
        (Player 
            <$> strOption 
                (long "player" <> short 'p' <> metavar "PLAYER" <> help "Filter matches to ones with PLAYER"))
    , many
        (Stage <$> strOption
                (long "stage" <> short 's' <> metavar "STAGE" <> help "Filter matches to ones on STAGE"))
    ]
   where timeFMT = "[yyyy-]mm-ddThh:mm[:ss][.sss]"
         -- so many time formats...
         parseTime timeStr = 
            let (startYear, _, _) = toGregorian day
                in asum
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
            

module Boss (
        parseCommand
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Text.Read (readMaybe)

import Options.Applicative

import Salmon
import Salmon.StatMap (StatMap)
import qualified Salmon.StatMap as SM

import Util.StatMap
import Util.CSV
import GHC.Natural (Natural)
import Data.List (sort)

data Flag = CSV
          | Sum
          | Mean
          | Median
          | Best
          deriving (Show, Eq, Ord, Bounded, Enum)

-- data passed on cmd line for the bosses command
data Data = BData 
          (Set Boss) 
          Flag
          Bool -- Should we sum up all bosses selected or not
                deriving (Show)

parseCommand :: Parser (RoundMap -> [Text])
parseCommand = handle <$> parser

parser :: Parser Data
parser = BData 
            -- build our set
            . S.fromList 
            <$> many (argument 
                 -- either read in the boss or try the string conversion function
                 -- to allow for both names
                 (maybeReader (\s -> readMaybe s <|> (fromText . T.pack) s))
                 (metavar "BOSS NAMES..." <> help "List of bosses to output stats on or nothing for all bosses"))
                -- flags for every option we can do
            <*>                (
                    flag' CSV  (long "csv" <> short 'c' <> help "Output boss kills in CSV format on stdout")
                <|> flag' Sum  (long "sum" <> short 's' <> help "Sum boss stats per boss to display")
                <|> flag' Mean (long "avg" <> short 'a' <> help "Average boss stats")
                <|> flag' Median (long "median" <> help "Compute median per boss statistic")
                <|> flag' Best (long "best" <> short 'b' <> help "display the best stats for each boss across selected games (best kills, best team kills, and best spawns)")
                )
            <*> flag False True (long "total" <> short 't' <> help "Display totals rather than per-boss statistics")


-- build output for each option
handle :: Data -> RoundMap -> [Text]
handle (BData selectedSet f shouldSum) m = 
    let selectedSet'   = if S.null selectedSet then S.fromList [minBound..] else selectedSet -- empty = all bosses
        -- a map of only the bosses we want to display
        selectedBosses = M.map ((SM.restrictKeys selectedSet') . bosses) $ m
     in if shouldSum 
     then case f of
             CSV    -> error "Cannot output totalled CSV" -- TODO: rule out in parser
             Sum    -> prettyPrintBossStats "TOTAL" . SM.statsFoldl' (+) 0 . statMapsSum $ selectedBosses
             Mean   -> prettyPrintBossStats @Double "TOTAL" . collapsedAvg $ selectedBosses
             Best   -> prettyPrintBossStats "TOTAL" 
                . M.foldl' maxBossStats (BS 0 0 0) . fmap (SM.statsFoldl' (+) 0) $ selectedBosses
             Median -> prettyPrintBossStats @(Maybe Double) "TOTAL" 
                . (fmap (calcMedian . sort)) . M.foldl' (flip consBossStats) (BS [] [] []) . fmap (SM.statsFoldl' (+) 0) $ selectedBosses
     else case f of
             CSV    -> toCSV bHeader blinePieces 3 selectedSet' selectedBosses
             Sum    -> prettyPrintBossMap . statMapsSum $ selectedBosses
             Mean   -> prettyPrintBossMap @Double . statMapsAvg $ selectedBosses
             Best   -> prettyPrintBossMap . statMapsMax $ selectedBosses
             Median -> prettyPrintBossMap @(Maybe Double) . statMapsMedian $ selectedBosses


collapsedAvg :: Map GameID (StatMap Boss BossStats Natural) -> BossStats Double
collapsedAvg m = let len = length m
        in fmap ((/ fromIntegral len) . fromIntegral) . SM.statsFoldr (+) 0 . fmap sum . SM.toStatsList $ m


bHeader k = let name = toText k
                in fmap ((name <> " ") <>) ["kills", "team kills", "spawns"]

blinePieces :: Show a => BossStats a -> [Text]
blinePieces (BS killed teamKilled spawned) = fmap textShow [killed, teamKilled, spawned]
    where textShow = T.pack . show

-- builds the header for the boss CSV output
-- each boss get's 3 columns: kills, team kills (which includes your own) and number spawned
makeHeader :: Set Boss -> Text
makeHeader = T.intercalate "\t"
            -- prepend GameID
            . ("Game ID," :) 
            -- add boss columns to line for each in the set
            . foldr (\boss headLine -> buildRows boss : headLine) []
    where buildRows b = let name = toText b
                            in name <> " kills,\t" <> name <> " team kills,\t" <> name <> " spawns,"


-- builds up CSV lines from the map of bosses
-- If there is no data for a boss (it wasn't in the wave) we show it as blank
buildLines :: (Show a) => Set Boss -> Map GameID (StatMap Boss BossStats a) -> [Text]
buildLines selSet bossMap = M.foldrWithKey (\i bm ls -> buildLine i bm : ls) [] bossMap
    where buildLine i bm = T.intercalate "\t"  -- combines bosses together with tabs
            -- adds the ID and comma to the front
            . (i <> "," :) 
            -- goes through the set and builds a string per selected boss
            . foldr (\b l -> (printBStat . flip SM.getStats bm) b : l) [] 
            $ selSet
          printBStat (Just (BS k tk s)) = packShow k <> ",\t" <> packShow tk <> ",\t" <> packShow s <> ","
          printBStat Nothing            = ",\t,\t," -- nothing there but same number of commas and tabs to align
          packShow = T.pack . show                

---- finds the largest statistic for each boss out of all the bosses
--findMax :: (Ord a) => Map k (StatMap Boss BossStats a) -> StatMap Boss BossStats a
--findMax = M.foldl' (SM.unionWith (liftA2 max)) SM.empty

-- prints a boss map nicely, 5 lines per boss counting empty lines for spacing
prettyPrintBossMap :: (Show a) => StatMap Boss BossStats a -> [Text]
prettyPrintBossMap = SM.foldMapWithKey (prettyPrintBossStats . toText)

-- prints a 
prettyPrintBossStats :: (Show a2) => Text -> BossStats a2 -> [Text]
prettyPrintBossStats boss (BS {kills=k, teamKills=t, spawned=s}) = 
        [   boss
        ,   "\t     Kills: " <> packShow k
        ,   "\tTeam Kills: " <> packShow t
        ,   "\t   Spawned: " <> packShow s
        ,   "" -- empty line
        ]
    where packShow = T.pack . show

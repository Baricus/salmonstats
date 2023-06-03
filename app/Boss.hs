module Boss (
        Boss.command, handle, Data
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Data.List ( sort, foldl' )

import Text.Read (readMaybe)

import Options.Applicative

import Salmon
import Salmon.BossMap (BossMap)
import qualified Salmon.BossMap as BM
import GHC.Natural (Natural)

data Flag = CSV
          | Sum
          | Mean
          | Median
          | Best
          deriving (Show, Eq, Ord, Bounded, Enum)

-- data passed on cmd line for the bosses command
data Data = BData (Set Boss) Flag
                deriving (Show)

command :: Parser Data
command = BData 
            -- build our set
            . S.fromList 
            <$> many (argument 
                 -- either read in the boss or try the string conversion function
                 -- to allow for both names
                 (maybeReader (\s -> readMaybe s <|> (textToBoss . T.pack) s))
                 (metavar "BOSS NAMES..." <> help "List of bosses to output stats on or nothing for all bosses"))
                -- flags for every option we can do
            <*>                (
                    flag' CSV  (long "csv" <> short 'c' <> help "Output boss kills in CSV format on stdout")
                <|> flag' Sum  (long "sum" <> short 's' <> help "Sum boss stats per boss to display")
                <|> flag' Mean (long "avg" <> short 'a' <> help "Average boss stats")
                <|> flag' Median (long "median" <> help "Compute median per boss statistic")
                <|> flag' Best (long "best" <> short 'b' <> help "display the best stats for each boss across selected games (best kills, best team kills, and best spawns)")
            )

-- build output for each option
handle :: Data -> RoundMap -> [Text]
handle (BData selectedSet f) m = 
    let selectedSet'   = if S.null selectedSet then S.fromList [minBound..] else selectedSet -- empty = all bosses
        -- a map of only the bosses we want to display
        selectedBosses = M.map ((`BM.restrictKeys` selectedSet') . bosses) $ m
     in
        case f of
             -- we need to make sure we go in order, so we fold over the selected set to line up the two parts
             CSV  -> [makeHeader selectedSet'] <> buildLines selectedSet' selectedBosses
             Sum  -> prettyPrintBossMap . buildSums $ selectedBosses
             Mean -> prettyPrintBossMap . avg $ selectedBosses
             Best -> prettyPrintBossMap . findMax $ selectedBosses
             Median -> prettyPrintBossMap . median $ selectedBosses

-- various helper functions to build up the handle function

-- builds the header for the boss CSV output
-- each boss get's 3 columns: kills, team kills (which includes your own) and number spawned
makeHeader :: Set Boss -> Text
makeHeader = T.intercalate "\t"
            -- prepend GameID
            . ("Game ID," :) 
            -- add boss columns to line for each in the set
            . foldr (\boss headLine -> buildRows boss : headLine) []
    where buildRows b = let name = bossToText b
                            in name <> " kills,\t" <> name <> " team kills,\t" <> name <> " spawns,"


-- builds up CSV lines from the map of bosses
-- If there is no data for a boss (it wasn't in the wave) we show it as blank
buildLines :: (Show a) => Set Boss -> Map GameID (BossMap a) -> [Text]
buildLines selSet bossMap = M.foldrWithKey (\i bm ls -> buildLine i bm : ls) [] bossMap
    where buildLine i bm = T.intercalate "\t"  -- combines bosses together with tabs
            -- adds the ID and comma to the front
            . (i <> "," :) 
            -- goes through the set and builds a string per selected boss
            . foldr (\b l -> (printBStat . flip BM.getStats bm) b : l) [] 
            $ selSet
          printBStat (Just (BS k tk s)) = packShow k <> ",\t" <> packShow tk <> ",\t" <> packShow s <> ","
          printBStat Nothing            = ",\t,\t," -- nothing there but same number of commas and tabs to align
          packShow = T.pack . show

-- folds two maps together into a map of total kills
-- anytime we don't see a boss, it's replaced with all 0's
buildSums :: (Num a) => Map k (BossMap a) -> BossMap a
buildSums = fmap (foldr (+) 0) . BM.toStatsList

-- converts a summed BossMap into an averaged BossMap
avg :: (Integral a) => Map k (BossMap a) -> BossMap Double
avg m = let len = M.size m
          in fmap ((/ fromIntegral len) . fromIntegral . foldr (+) 0) . BM.toStatsList $ m

median :: Map k (BossMap Natural) -> BossMap (Maybe Double)
median = fmap (calcMedian . sort) . BM.toStatsList
    where calcMedian :: [Natural] -> Maybe Double
          calcMedian [] = Nothing
          calcMedian l | odd (length l) = Just . fromIntegral $ l !! (length l `div` 2)
                       | otherwise      = let len = length l `div` 2
                      in Just . (\e -> fromIntegral e / 2) $ foldl' (+) (0) ([l !! len, l !! (len + 1)] :: [Natural])

-- finds the largest statistic for each boss out of all the bosses
findMax :: (Ord a) => Map k (BossMap a) -> BossMap a
findMax = M.foldl' (BM.unionWith pickMax) BM.empty 
    where pickMax :: (Ord a) => (BossStats a) -> (BossStats a) -> (BossStats a)
          pickMax (BS ak atk as) (BS bk btk bs) = BS (max ak bk) (max atk btk) (max as bs)

-- prints a boss map nicely, 5 lines per boss counting empty lines for spacing
prettyPrintBossMap :: (Show a) => BossMap a -> [Text]
prettyPrintBossMap = BM.foldMapWithKey buildLine
    where buildLine boss (BS {kills=k, teamKills=t, spawned=s}) = 
            [   bossToText boss
            ,   "\t     Kills: " <> packShow k
            ,   "\tTeam Kills: " <> packShow t
            ,   "\t   Spawned: " <> packShow s
            ,   "" -- empty line
            ]
          packShow = T.pack . show

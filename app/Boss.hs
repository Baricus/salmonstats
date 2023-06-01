module Boss where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Text.Read (readMaybe)

import Options.Applicative

import Salmon
import Data.Maybe (fromMaybe)

data Flag = CSV
          | Sum
          | Avg
          | Best
          deriving (Show, Eq, Ord, Bounded, Enum)

-- data passed on cmd line for the bosses command
data Data = BData (Set Boss) Flag
                deriving (Show)

command :: Parser Data
command = BData 
            -- build our set
            . S.fromList <$> many (argument 
                 -- either read in the boss or try the string conversion function
                 -- to allow for both names
                 (maybeReader (\s -> readMaybe s <|> (textToBoss . T.pack) s))
                 (metavar "BOSS NAMES..." <> help "List of bosses to output stats on or nothing for all bosses"))
            -- flags for every option we can do
            <*> (
                flag' CSV  (long "csv" <> short 'c' <> help "Output boss kills in CSV format on stdout")
            <|> flag' Sum  (long "sum" <> short 's' <> help "Sum boss stats per boss to display")
            <|> flag' Avg  (long "avg" <> short 'a' <> help "Average boss stats and display")
            <|> flag' Best (long "best" <> short 'b' <> help "display the best stats for each boss across selected games")
            )

-- build output for each option
handle :: Data -> RoundMap -> [Text]
handle (BData selectedSet f) m = 
    let selectedSet'   = if S.null selectedSet then S.fromList [minBound..] else selectedSet -- empty = all bosses
        -- a map of only the bosses we want to display
        selectedBosses = M.map ((`M.restrictKeys` selectedSet') . bosses) $ m
     in
        case f of
             CSV -> [makeHeader selectedBosses] <> buildLines selectedBosses
             Sum -> prettyPrintBossMap . M.foldl' buildSums bossMapEmpty $ selectedBosses
             _   -> undefined

-- builds the header for the boss CSV output
-- each boss get's 3 columns: kills, team kills (which includes your own) and number spawned
-- TODO: try turning this into a foldr and ensure everything is still ordered the same (don't reverse the list)
makeHeader :: Map GameID BossMap -> Text
makeHeader = T.intercalate "\t" . M.foldlWithKey' (\headLine boss _ -> headLine <> [buildRows boss]) [] . head . M.elems
    where buildRows b = let name = bossToText b
                            in name <> " kills,\t" <> name <> " team kills,\t" <> name <> " spawns,"


-- builds up CSV lines from the map of bosses
-- If there is no data for a boss (it wasn't in the wave) we show it as blank
buildLines :: Map GameID BossMap -> [Text]
buildLines = fmap (T.intercalate "\t" . fmap ( printBM) . M.elems) . M.elems
    where printBM (Just (BS k tk s)) = packShow k <> ",\t" <> packShow tk <> ",\t" <> packShow s <> ","
          printBM Nothing            = ",\t,\t," -- nothing there but same number of commas and tabs
          packShow = T.pack . show

-- folds two maps together into a map of total kills
buildSums :: BossMap -> BossMap -> BossMap
buildSums = M.unionWith (\l r -> Just $ sumBossKills (fromMaybe (BS 0 0 0) l) (fromMaybe (BS 0 0 0) r))

prettyPrintBossMap :: BossMap -> [Text]
prettyPrintBossMap = M.foldMapWithKey buildLine
    where buildLine _ Nothing   = []
          buildLine boss (Just BS {kills=k, teamKills=t, spawned=s}) = 
            [   bossToText boss
            ,   "\t     Kills: " <> packShow k
            ,   "\tTeam Kills: " <> packShow t
            ,   "\t   Spawned: " <> packShow s
            ,   "" -- empty line
            ]
          packShow = T.pack . show

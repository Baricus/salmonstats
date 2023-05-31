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

-- data passed on cmd line for the bosses command
data BossesData = BData (Set Boss) Flag

bossCmd :: Parser BossesData
bossCmd = BData 
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
handle :: BossesData -> RoundMap -> [Text]
handle (BData selectedSet f) m = 
    let selectedSet'   = if S.null selectedSet then S.fromList [minBound..] else selectedSet -- empty = all bosses
        selectedBosses = M.map ((`M.restrictKeys` selectedSet') . bosses) $ m
     in
        case f of
             CSV -> [makeHeader selectedBosses] <> buildLines selectedBosses
             Sum -> prettyPrintBossMap . M.foldl' buildSums bossMapEmpty $ selectedBosses
             _   -> undefined

makeHeader :: Map GameID BossMap -> Text
makeHeader = undefined

buildLines :: Map GameID BossMap -> [Text]
buildLines = undefined

buildSums :: BossMap -> BossMap -> BossMap
buildSums = M.unionWith (\l r -> Just $ sumBossKills (fromMaybe (BS 0 0 0) l) (fromMaybe (BS 0 0 0) r))

prettyPrintBossMap :: BossMap -> [Text]
prettyPrintBossMap = undefined

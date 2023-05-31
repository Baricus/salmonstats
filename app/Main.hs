module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Set (Set)
import qualified Data.Set as S

import Text.Read (readMaybe)

import Options.Applicative

import qualified Data.Csv as Csv
import qualified Data.Csv.Incremental as Csv

import Data.Either (rights)
import Data.Maybe (fromMaybe)

import Salmon

import qualified Boss as Boss

data Commands = Bosses Boss.Data
              | Kings (Set Text)
            deriving (Show)

kingCmd :: Parser Commands
kingCmd = Kings . S.fromList 
            <$> many (argument str (metavar "KING NAMES..." <> help "List of kings to print statistics on or nothing for all kings"))

opts :: Parser Commands
opts = subparser
        (  command "bosses" (info (helper <*> (Bosses <$> Boss.command)) (progDesc "Prints out boss kills for requested bosses"))
        <> command "kings"  (info (helper <*> kingCmd) (progDesc "Prints out stats for kings"))
        )

argParser :: ParserInfo Commands
argParser = info (helper <*> opts) 
                 (fullDesc <> progDesc "Outputs various salmon run stats" <> header "Hello parser!")


printBosses :: Set Boss -> Round -> IO ()
printBosses bset CR{bosses=bmap} = 
    let printBossStats (BS k tk s) = T.putStr . (<> ",\t") . T.intercalate ",\t" . fmap (T.pack . show) $ [k, tk, s]
        in do
            -- we always have all keys in the boss map bmap by construction; it's total on the Boss type
            -- However, it stores Maybe BossStats instead!
            mapM_ (\boss -> printBossStats . fromMaybe (BS 0 0 0) . bossMapGetStats boss $ bmap) bset
            T.putStrLn ""

--printRounds :: IO () -> (Round -> IO ()) -> [Round] -> IO ()
--printRounds headline f r = headline *> T.putStrLn "" *> mapM_ f r

-- format notes:
-- Args should have general options like a list of filters (dates, players, etc) 
--
-- Each command should have flags saying what to do with the data (sum, csv, average, trends, etc)
--
-- No commands could open up a TUI(?) data explorer app where all this is dynamic?
-- Would be cool but currently beyond the scope

main :: IO ()
main = do
    args <- execParser argParser 
    rounds <- toIDMap . rights <$> readRoundsFromNXAPIdir "splatnet3/"
    mapM_ T.putStrLn $ case args of
            (Bosses bdata) -> Boss.handle bdata rounds
                --printRounds 
                --(mapM_ (T.putStr . (<> ",\t,\t,\t") . bossToText) (if S.null b then S.fromList [minBound..] else b)) 
                --(printBosses (if S.null b then S.fromList [minBound..] else b))
            
            (Kings  _) -> error "not implemented!"

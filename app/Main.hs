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

import Salmon
import Data.Either (rights)
import Data.Maybe (fromMaybe)


data Commands = Bosses (Set Boss)
              | Kings (Set Text)
            deriving (Show)

bossCmd :: Parser Commands
bossCmd = Bosses . S.fromList 
            <$> many (argument 
             -- either read in the boss or try the string conversion
             (maybeReader (\s -> readMaybe s <|> (textToBoss . T.pack) s))
             (metavar "BOSS NAMES..." <> help "List of bosses to output stats on or nothing for all bosses"))

kingCmd :: Parser Commands
kingCmd = Kings . S.fromList 
            <$> many (argument str (metavar "KING NAMES..." <> help "List of kings to print statistics on or nothing for all kings"))

opts :: Parser Commands
opts = subparser
        (  command "bosses" (info (helper <*> bossCmd) (progDesc "Prints out boss kills for requested bosses"))
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


printRounds :: IO () -> (Round -> IO ()) -> [Round] -> IO ()
printRounds headline f r = headline *> T.putStrLn "" *> mapM_ f r

main :: IO ()
main = do
    args <- execParser argParser 
    rounds <- rights <$> readRoundsFromNXAPIdir "splatnet3/"
    let f = case args of
            (Bosses b) -> printRounds 
                (mapM_ (T.putStr . (<> ",\t,\t,\t") . bossToText) (if S.null b then S.fromList [minBound..] else b)) 
                (printBosses (if S.null b then S.fromList [minBound..] else b))
            
            (Kings  _) -> error "not implemented!"
    f rounds

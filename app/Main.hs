module Main where

import qualified Data.Text.IO as T

import qualified Data.Set as S

import Options.Applicative

import Data.Either (partitionEithers)

import Salmon (readRoundsFromNXAPIdir, toIDMap, readShiftsFromNXAPIdir, shiftInfo)

import Data.Time (UTCTime, TimeZone, getCurrentTime, getCurrentTimeZone)
import Data.Time.Format (TimeLocale, defaultTimeLocale)

import qualified Boss as Boss
import qualified King as King

import qualified Filters as Filters


data Command = Bosses Boss.Data
             | Kings King.Data
            deriving (Show)

data Args = A
          { dataDir :: FilePath
          , filters :: Filters.Data
          , comm    :: Command
          }


commands :: Parser Command
commands = subparser
        (  command "bosses" (info (helper <*> (Bosses <$> Boss.command)) (progDesc "Prints out boss kills for requested bosses"))
        <> command "kings"  (info (helper <*> (Kings <$> King.command)) (progDesc "Prints out stats for kings"))
        )

argParser :: UTCTime -> TimeZone -> TimeLocale -> ParserInfo Args
argParser time zone local = info (helper <*>
                 -- TODO: change default to nxapi data directory (and move my data over)
                 (A <$> strOption (value "splatnet3" <> showDefault <> metavar "DIRECTORY" <> long "dir" <> short 'd' <> help "Directory which contains the nxapi salmon run results") 
                    <*> Filters.opts time zone local
                    <*> commands))
                 (fullDesc <> progDesc "Outputs various salmon run stats")

-- format notes:
-- Args should have general options like a list of filters (dates, players, etc) 
--
-- Each command should have flags saying what to do with the data (sum, csv, average, trends, etc)
--
-- No commands could open up a TUI(?) data explorer app where all this is dynamic?
-- Would be cool but currently beyond the scope

main :: IO ()
main = do
    -- get time info to pass on to the parser
    time <- getCurrentTime
    zone <- getCurrentTimeZone
    A {dataDir=dir, filters=filt, comm=comm} <- execParser $ argParser time zone defaultTimeLocale
    -- read in all the data
    (errors, rounds) <- fmap toIDMap . partitionEithers <$> readRoundsFromNXAPIdir dir
    -- print any errors we find
    mapM_ print errors
    -- also read in the shifts
    (errors', shifts) <- partitionEithers <$> readShiftsFromNXAPIdir dir
    mapM_ print errors'

    mapM_ print . (fmap . fmap) shiftInfo . fmap S.toList $ shifts
    -- filter rounds
    let filteredRounds = Filters.filterRounds filt rounds
    -- handle whatever command is given on the command line
    mapM_ T.putStrLn $ case comm of
            (Bosses bdata) -> Boss.handle bdata filteredRounds
            (Kings  kdata) -> King.handle kdata filteredRounds

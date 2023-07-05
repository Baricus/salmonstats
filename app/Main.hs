module Main where

import qualified Data.Text.IO as T

import Options.Applicative

import Data.Either (partitionEithers)

import Salmon (readRoundsFromNXAPIdir, toIDMap, readShiftsFromNXAPIdir, toIDShiftMap, addShiftData, RoundMap)

import Data.Time (UTCTime, TimeZone, getCurrentTime, getCurrentTimeZone)
import Data.Time.Format (TimeLocale, defaultTimeLocale)

import qualified Filters as Filters

import Boss 
import King 
import Data.Text (Text)
import Command (buildCommand)

data Args = A
          { dataDir     :: FilePath
          , filters     :: Filters.Data
          , tuiCommand  :: RoundMap -> [Text]
          }

commands :: Parser (RoundMap -> [Text])
commands = subparser
        (  buildCommand "bosses" Boss.parseCommand "Prints out boss kills for requested bosses"
        <> buildCommand "kings"  King.parseCommand "Prints out stats for kings"
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
    A {dataDir=dir, filters=filt, tuiCommand=execCommand} <- execParser $ argParser time zone defaultTimeLocale
    -- read in the shifts
    (sErrors, shifts) <- fmap toIDShiftMap . partitionEithers <$> readShiftsFromNXAPIdir dir
    mapM_ print sErrors
    -- read in all the game data and add in the shift information
    (rErrors, rounds) <- fmap (addShiftData shifts . toIDMap) . partitionEithers <$> readRoundsFromNXAPIdir dir
    -- print any errors we find
    mapM_ print rErrors
    -- filter rounds
    let filteredRounds = Filters.filterRounds filt rounds
    -- handle whatever command is given on the command line
    mapM_ T.putStrLn $ execCommand filteredRounds

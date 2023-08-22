module Main where

import qualified Data.Text.IO as T

import qualified Data.Map as M

import Options.Applicative

import Data.Either (partitionEithers)

import Salmon (readRoundsFromNXAPIdir, toIDMap, readShiftsFromNXAPIdir, toIDShiftMap, addShiftData, RoundMap)

import Data.Time (UTCTime, TimeZone, getCurrentTime, getCurrentTimeZone)
import Data.Time.Format (TimeLocale, defaultTimeLocale)

import qualified Filters

import Boss 
import King 
import Data.Text (Text)
import Command (buildCommand)

import System.FilePath
import System.Directory 

import Data.Ini.Config

import Config ( Config (..), configParser, defaultConfig )

data Args = A
          { dataDir     :: FilePath
          , roundFilter :: RoundMap -> RoundMap
          , shouldIgnoreOffsets :: Bool
          , tuiCommand  :: RoundMap -> [Text]
          }

commands :: Parser (RoundMap -> [Text])
commands = subparser
        (  buildCommand "bosses" Boss.parseCommand "Prints out boss kills for requested bosses"
        <> buildCommand "kings"  King.parseCommand "Prints out stats for kings"
        )


getDefaultDataDir :: IO FilePath
getDefaultDataDir = getXdgDirectory XdgData end
    where end = "nxapi-nodejs/splatnet3"
                
getConfigDirectory :: IO FilePath
getConfigDirectory = getXdgDirectory XdgConfig "salmonstats"

argParser :: FilePath -> UTCTime -> TimeZone -> TimeLocale -> ParserInfo Args
argParser defaultDataDir time zone local = info (helper <*>
                 -- TODO: change default to nxapi data directory (and move my data over)
                 (A <$> strOption (value defaultDataDir <> showDefault <> metavar "DIRECTORY" <> long "dir" <> short 'd' <> help "Directory which contains the nxapi salmon run results") 
                    <*> Filters.opts time zone local
                    <*> switch (long "ignore-offsets" <> help "Ignore offsets for boss counts stored in the config file")
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
    -- parses config if it exists
    configDir <- getConfigDirectory
    createDirectoryIfMissing True configDir
    let configPath = configDir </> "config"
    configExists <- doesFileExist configPath
    Config {offsets=offsetR} <- if configExists
                       then either error id . (flip parseIniFile configParser) <$> T.readFile configPath 
                       else pure defaultConfig

    -- get command line args
    time <- getCurrentTime
    zone <- getCurrentTimeZone
    defaultDataDir <- getDefaultDataDir
    A {dataDir=dir, roundFilter=filt, tuiCommand=execCommand, shouldIgnoreOffsets=shouldIgnoreOffsets} <- 
            execParser $ argParser defaultDataDir time zone defaultTimeLocale

    -- read in the shifts
    (sErrors, shifts) <- fmap toIDShiftMap . partitionEithers <$> readShiftsFromNXAPIdir dir
    mapM_ print sErrors
    -- read in all the game data and add in the shift information
    (rErrors, rounds) <- fmap (addShiftData shifts . toIDMap) . partitionEithers <$> readRoundsFromNXAPIdir dir
    -- print any errors we find
    mapM_ print rErrors

    -- add in offset round if needed
    let rounds' = if shouldIgnoreOffsets 
                     then rounds 
                     else M.insert "OFFSETROUND" offsetR rounds 

    -- filter rounds
    let filteredRounds = filt rounds'
    -- handle whatever command is given on the command line
    mapM_ T.putStrLn $ execCommand filteredRounds

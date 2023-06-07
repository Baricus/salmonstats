module Salmon (
    module Salmon.Round,
    module Salmon.Wave,
    module Salmon.Boss,
    module Salmon.King,
    module Salmon.WaterLevel,
    module Textworthy,

    -- functions to parse nxapi data into Rounds objects
    readRoundFromNintendoFile,
    readRoundsFromNXAPIdir,
    ) where

import Salmon.Round
import Salmon.Wave
import Salmon.Boss
import Salmon.King
import Salmon.WaterLevel

import Textworthy
import Salmon.NintendoJSON ( readNintendoJSONFile )

import System.Directory ( listDirectory )
import Data.List (isPrefixOf)

readRoundFromNintendoFile :: FilePath -> IO (Either String Round)
readRoundFromNintendoFile = readNintendoJSONFile

readRoundsFromNXAPIdir :: FilePath -> IO [Either String Round]
readRoundsFromNXAPIdir folder = do
    paths <- listDirectory folder
    -- only grab the files that are coop results
    let filtered  = filter (isPrefixOf "splatnet3-coop-result-u") paths
        fullpaths = ((folder <> "/") <>) <$> filtered
    -- read all the files in that folder
    mapM readRoundFromNintendoFile fullpaths

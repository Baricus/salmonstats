module Salmon (
    module Salmon.Round,
    module Salmon.Wave,
    module Salmon.Boss,
    module Salmon.WaterLevel,

    readRoundFromNintendoFile
    ) where

import Salmon.Round
import Salmon.Wave
import Salmon.Boss
import Salmon.WaterLevel

import Salmon.NintendoJSON

readRoundFromNintendoFile :: FilePath -> IO (Either String Round)
readRoundFromNintendoFile = readNintendoJSONFile

module Config (
    defaultConfig,
    configParser,
    Config(..),
    ) where

import Data.Ini.Config

import Salmon

import Config.Offsets


data Config = Config 
            { offsets :: Round
            }

configParser :: IniParser Config
configParser = Config <$> offsetConfig

defaultConfig :: Config
defaultConfig = Config $ offsetRound defaultOffsets

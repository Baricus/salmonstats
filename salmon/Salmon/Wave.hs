module Salmon.Wave (
        -- What kind of salmon run global events are going on
        Event,
        -- All information about an individual wave
        WaveStats(..),
    ) where

import Salmon.NintendoJSON
import Salmon.WaterLevel

import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Natural
import GHC.Generics

import Data.Aeson
import Data.Aeson.Types ( Parser )

-- TODO: convert to datatype
type Event = Text

data WaveStats = WS
          {
              number       :: Natural
            , level        :: WaterLevel
            , event        :: Maybe Event
            , quota        :: Maybe Natural -- boss waves have no quota
            , teamEggs     :: Maybe Natural
            , spawnedEggs  :: Natural
            , specialsUsed :: Vector Text
          }
      deriving (Show, Generic)

instance ToJSON WaveStats where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON WaveStats where

instance FromNintendoJSON WaveStats where
    parseNJSON = withObject "wave" $ \obj -> do
        num      <- obj .: "waveNumber"
        level    <- (obj .: "waterLevel" :: Parser Natural) >>= \cases
                         0 -> pure Low
                         1 -> pure Normal
                         2 -> pure High
                         _ -> fail "Invalid water level"
        event    <- obj .:? "eventWave" >>= traverse (withObject "event" (.: "name"))
        -- these two are optional because of boss waves
        eggs     <- obj .:? "deliverNorm"
        teamE    <- obj .:? "teamDeliverCount"
        spawnE   <- obj .: "goldenPopCount"
        specials <- obj .: "specialWeapons" >>= traverse (withObject "special" (.: "name"))
        pure $ WS num level event eggs teamE spawnE specials

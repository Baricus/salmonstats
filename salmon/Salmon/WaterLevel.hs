module Salmon.WaterLevel where

import Data.Aeson
    ( defaultOptions, genericToEncoding, FromJSON, ToJSON(toEncoding) )
import GHC.Generics ( Generic )

data WaterLevel = Low | Normal | High
                deriving (Show, Generic)

instance ToJSON WaterLevel where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON WaterLevel where

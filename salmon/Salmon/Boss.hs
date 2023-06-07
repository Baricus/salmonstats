module Salmon.Boss (
    -- * All possible Salmon Run bosses
    Boss(..),
    -- Player statistics relating to a boss
    BossStats(..),
    -- Helper functions to work with boss objects
    sumBossStats,
    ) where

import Salmon.NintendoJSON
import Textworthy

import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics ( Generic )
import GHC.Natural ( Natural ) 

import Text.Read (readMaybe)

import Data.Aeson
import Data.String (IsString (fromString))

data Boss = Steelhead
          | Flyfish
          | SteelEel
          | Drizzler
          | Stinger
          | Scrapper
          | Maws
          | FishStick
          | FlipperFlopper
          | SlamminLid
          | BigShot
          | Goldie
          | Griller
          | Mudmouth
        deriving (Read, Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Boss where
    toEncoding = genericToEncoding defaultOptions

instance ToJSONKey Boss where
instance FromJSONKey Boss where

instance FromJSON Boss where

instance Textworthy Boss where
    toText :: IsString a => Boss -> a
    toText = \cases
        SteelEel       -> "Steel Eel"
        FishStick      -> "Fish Stick"
        FlipperFlopper -> "Flipper-Flopper"
        SlamminLid     -> "Slammin' Lid"
        BigShot        -> "Big Shot"
        -- take advantage of the default show since it matches our names
        b              -> fromString . show $ b

    fromText :: Text -> Maybe Boss
    fromText = \cases
        "Steel Eel"       -> Just SteelEel
        "Fish Stick"      -> Just FishStick
        "Flipper-Flopper" -> Just FlipperFlopper
        "Slammin' Lid"    -> Just SlamminLid
        "Big Shot"        -> Just BigShot
        -- take advantage of the default read instance since they match our names
        t                 -> readMaybe . T.unpack $ t

-- The 3 pieces of data we get about a boss after a round
data BossStats a = BS
               {
                   kills     :: a 
                 , teamKills :: a
                 , spawned   :: a -- TODO: check if this is what popCount means!
               }
            deriving (Show, Generic)

instance (ToJSON a) => ToJSON (BossStats a) where
    toEncoding = genericToEncoding defaultOptions

instance (FromJSON a) => FromJSON (BossStats a) where

instance FromNintendoJSON (BossStats Natural) where
    parseNJSON = withObject "boss" $ \obj -> 
        BS <$> (obj .: "defeatCount") <*> (obj .: "teamDefeatCount") <*> (obj .: "popCount")

instance Functor BossStats where
    fmap f (BS k tk sp) = BS (f k) (f tk) (f sp)

instance Applicative BossStats where
    pure a = BS a a a
    (<*>) :: BossStats (a -> b) -> BossStats a -> BossStats b
    (<*>) (BS fk ftk fsp) (BS ak atk asp) = BS (fk ak) (ftk atk) (fsp asp)

-- No monad instance; it wouldn't make sense


sumBossStats :: (Num a) => BossStats a -> BossStats a -> BossStats a
sumBossStats (BS k tk s) (BS k' tk' s') = BS (k + k') (tk + tk') (s + s')

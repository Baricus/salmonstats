module Salmon.Boss (
    -- * All possible Salmon Run bosses
    Boss(..),
    -- Player statistics relating to a boss
    BossStats(..),
    -- Helper functions to convert between bosses and official name strings
    bossToText,
    textToBoss,
    -- Helper functions to work with boss objects
    sumBossKills,
    -- A boss map is a total map over bosses, containing optional BossStats.
    BossMap,
    -- These functions create and alter these maps
    bossMapEmpty,
    bossMapInsertStats,
    bossMapGetStats,
    -- * King salmonoids
    King(..),
    ) where

import Salmon.NintendoJSON

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import GHC.Generics ( Generic )
import GHC.Natural ( Natural ) 

import Text.Read (readMaybe)

import Data.Aeson
import Data.String (IsString (fromString))
import Control.Monad (join)

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

-- this one is more general to allow any output!
bossToText :: IsString a => Boss -> a
bossToText = \cases
    SteelEel       -> "Steel Eel"
    FishStick      -> "Fish Stick"
    FlipperFlopper -> "Flipper-Flopper"
    SlamminLid     -> "Slammin' Lid"
    BigShot        -> "Big Shot"
    -- take advantage of the default show since it matches our names
    b              -> fromString . show $ b

textToBoss :: Text -> Maybe Boss
textToBoss = \cases
    "Steel Eel"       -> Just SteelEel
    "Fish Stick"      -> Just FishStick
    "Flipper-Flopper" -> Just FlipperFlopper
    "Slammin' Lid"    -> Just SlamminLid
    "Big Shot"        -> Just BigShot
    -- take advantage of the default read instance since they match our names
    t                 -> readMaybe . T.unpack $ t

-- The 3 pieces of data we get about a boss after a round
data BossStats = BS
               {
                   kills     :: Natural
                 , teamKills :: Natural
                 , spawned   :: Natural -- TODO: check if this is what popCount means!
               }
            deriving (Show, Generic)

instance ToJSON BossStats where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BossStats where

instance FromNintendoJSON BossStats where
    parseNJSON = withObject "boss" $ \obj -> 
        BS <$> (obj .: "defeatCount") <*> (obj .: "teamDefeatCount") <*> (obj .: "popCount")

sumBossKills :: BossStats -> BossStats -> BossStats
sumBossKills (BS k tk s) (BS k' tk' s') = BS (k + k') (tk + tk') (s + s')


-- some functions for boss maps
type BossMap = Map Boss (Maybe BossStats)
-- pulled out to cache this and have it at compile time
bossMapEmpty :: BossMap
bossMapEmpty = M.fromDistinctAscList [(k, Nothing) | k <- [minBound..]]

bossMapInsertStats :: Boss -> BossStats -> BossMap -> BossMap
bossMapInsertStats b s = M.insert b $ Just s

bossMapGetStats :: Boss -> BossMap -> Maybe BossStats
bossMapGetStats b m = join . M.lookup b $ m

-- King salmonoids!
data King = K
          {
              name   :: Text
            , killed :: Bool
            , bronze :: Natural
            , silver :: Natural
            , gold   :: Natural
          }
        deriving (Show, Generic)

instance ToJSON King where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON King where

-- parses this from the root object so we can grab the two relevant pieces
--
-- This is a bit weird, but it alowws us to combine the two pieces of data
-- (boss and scales) into one object that either exists or doesn't
-- rather than two
instance FromNintendoJSON King where
    parseNJSON = withObject "result" $ \obj -> do
        -- these are optional in the Nintendo JSON, but we parse it anyways
        bossRes <- obj .: "bossResult"
        scales  <- obj .: "scale"

        -- construct our final object
        K <$> (bossRes .: "boss" >>= (.: "name"))
          <*> bossRes .: "hasDefeatBoss"
          <*> scales  .: "bronze"
          <*> scales  .: "silver"
          <*> scales  .: "gold"

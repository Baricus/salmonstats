{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Round where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as M

import GHC.Generics
import GHC.Natural 

import Data.Foldable
import Control.Applicative

import Data.Time

import Data.Aeson
import Data.Aeson.Types

getName :: (FromJSON a) => Object -> Parser a
getName = (.: "name")

data CoopRound = CR
               {
                   gameID     :: Text
                 , time       :: UTCTime
                 , stage      :: Text
                 , hazard     :: Double
                 , player     :: Text
                 , weapons    :: Vector Text
                 , special    :: Text
                 , eggs       :: Natural
                 , eggAssists :: Natural
                 , rescues    :: Natural
                 , deaths     :: Natural
                 , waves      :: Vector WaveStats
                 , bosses     :: Map Text BossStats
                 , king       :: Maybe King
               }
            deriving (Show, Generic)

data WaterLevel = Low | Normal | High
                deriving (Show, Generic)

instance ToJSON WaterLevel where
    toEncoding = genericToEncoding defaultOptions

-- TODO: convert to datatype
type Event = Text

data BossStats = BS
               {
                   kills     :: Natural
                 , teamKills :: Natural
                 , spawned   :: Natural
               }
            deriving (Show, Generic)

instance ToJSON BossStats where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BossStats where
    parseJSON = withObject "boss" $ \obj -> 
        BS <$> (obj .: "defeatCount") <*> (obj .: "teamDefeatCount") <*> (obj .: "popCount")

data King = K
          {
              name   :: Text
            , killed :: Bool
            , bronze :: Natural
            , silver :: Natural
            , gold   :: Natural
          }
        deriving (Show, Generic)

-- parses this from the root object so we can grab the two relevant pieces
instance FromJSON King where
    parseJSON = withObject "result" $ \obj -> do
        -- these are optional in the Nintendo JSON, but we parse it anyways
        bossRes <- obj .: "bossResult"
        scales  <- obj .: "scale"

        -- construct our final object
        K <$> (bossRes .: "boss" >>= getName)
          <*> bossRes .: "hasDefeatBoss"
          <*> scales  .: "bronze"
          <*> scales  .: "silver"
          <*> scales  .: "gold"

instance ToJSON King where
    toEncoding = genericToEncoding defaultOptions

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
    parseJSON = withObject "wave" $ \obj -> do
        num      <- obj .: "waveNumber"
        level    <- (obj .: "waterLevel" :: Parser Natural) >>= \cases
                         0 -> pure Low
                         1 -> pure Normal
                         2 -> pure High
                         _ -> fail "Invalid water level"
        event    <- obj .:? "eventWave" >>= traverse (withObject "event" getName) :: Parser (Maybe Event)
        -- these two are optional because of boss waves
        eggs     <- obj .:? "deliverNorm"
        teamE    <- obj .:? "teamDeliverCount"
        spawnE   <- obj .: "goldenPopCount"
        specials <- obj .: "specialWeapons" >>= traverse (withObject "special" getName)
        pure $ WS num level event eggs teamE spawnE specials


-- generic toJSON that drops all irrelevant data
instance ToJSON CoopRound where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CoopRound where
    parseJSON = withObject "CoopRound" $ \obj -> do
        res        <- obj .: "result"
        myRes   <- res .: "myResult"

        gameID     <- res .: "id"
        time       <- res .: "playedTime"
        stage      <- res .: "coopStage" >>= getName
        hazard     <- res .: "dangerRate"
        username   <- myRes .: "player" >>= getName
        weapons    <- myRes .: "weapons" >>= withArray "weapons" (traverse (withObject "weapon" getName))
        special    <- myRes .: "specialWeapon" >>= getName
        eggs       <- myRes .: "goldenDeliverCount"
        eggAssists <- myRes .: "goldenAssistCount"
        rescues    <- myRes .: "rescueCount"
        deaths     <- myRes .: "rescuedCount"

        waves      <- res .: "waveResults" >>= traverse parseJSON

        bosses     <- res .: "enemyResults" >>= V.foldl
                    (\map val -> withObject "bossStats" 
                        (\obj -> 
                        M.insert <$> (obj .: "enemy" >>= getName) <*> parseJSON val <*> map) 
                        val)
                    (pure M.empty)

        -- we either parse the king or just give nothing back since it wasn't present
        king <- (parseJSON (Object res) <|> pure Nothing)

        -- man, this is an object       
        pure $ CR gameID time stage hazard 
                  username weapons special 
                  eggs eggAssists 
                  rescues deaths 
                  waves 
                  bosses king


-- parse results from a results file
parseResultFile :: FilePath -> IO (Either String CoopRound)
parseResultFile = eitherDecodeFileStrict

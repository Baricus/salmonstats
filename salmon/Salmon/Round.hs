module Salmon.Round (
    Round(..),
    -- All statistics about a single round of Salmon Run
    ) where

import Salmon.NintendoJSON
import Salmon.Boss
import Salmon.Wave

import Data.Text (Text)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as M

import GHC.Generics ( Generic )
import GHC.Natural ( Natural ) 

import Control.Applicative ( Alternative((<|>)) )

import Data.Time ( UTCTime )

import Data.Aeson
import Data.Aeson.Types ( Parser )

data Round = CR
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
                 , bosses     :: Map Boss BossStats
                 , king       :: Maybe King
               }
            deriving (Show, Generic)

instance ToJSON Round where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Round where

getName :: (FromJSON a) => Object -> Parser a
getName = (.: "name")

instance FromNintendoJSON Round where
    parseNJSON = withObject "Round" $ \obj -> do
        res        <- obj .: "result"
        myRes      <- res .: "myResult"

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

        waves      <- res .: "waveResults" >>= traverse parseNJSON

        bosses     <- res .: "enemyResults" >>= V.foldl
                    (\m val -> withObject "bossStats" 
                        (\statObj -> 
                            M.insert 
                            -- complex map to turn bosses into the Haskell data counterparts
                            -- translating the string representations
                            <$> (statObj .: "enemy" >>= getName >>= maybe (fail "Unknown Boss") pure . textToBoss
                                    :: Parser Boss) 
                            <*> parseNJSON val 
                            <*> m) 
                        val)
                    (pure M.empty) :: Parser (Map Boss BossStats)

        -- we either parse the king or just give nothing back since it wasn't present
        king <- (fmap Just (parseNJSON (Object res)) <|> pure Nothing)

        -- man, this is an object       
        pure $ CR gameID time stage hazard 
                  username weapons special 
                  eggs eggAssists 
                  rescues deaths 
                  waves 
                  bosses king

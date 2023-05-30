module Salmon.Round (
    -- All statistics about a single round of Salmon Run
    Round(..),
    GameID, RoundMap,
    -- Functions for managing round maps
    toIDMap, getIDs, nextRound, prevRound,
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

import Control.Applicative ( Alternative((<|>))) 

import Data.Time ( UTCTime )

import Data.Aeson
import Data.Aeson.Types ( Parser )
import Data.List (sortOn)

type GameID = Text
type RoundMap = Map GameID Round

data Round = CR
               {
                   gameID        :: Text
                 , time          :: UTCTime
                 , stage         :: Text
                 , hazard        :: Double
                 , player        :: Text
                 , team          :: Vector Text -- TODO: decide if this should be more info or not
                 , playedWeapons :: Vector Text
                 , allWeapons    :: Vector Text
                 , special       :: Text
                 , eggs          :: Natural
                 , eggAssists    :: Natural
                 , rescues       :: Natural
                 , deaths        :: Natural
                 , waves         :: Vector WaveStats
                 , bosses        :: Map Boss (Maybe BossStats) -- not all bosses are present always
                 , king          :: Maybe King
                 , nextHist      :: GameID
                 , prevHist      :: GameID
               }
            deriving (Show, Generic)

-- map of IDs to rounds
toIDMap :: [Round] -> RoundMap
toIDMap rounds = M.fromList [(gameID x, x) | x <- rounds]

-- get's IDs in time order
getIDs :: RoundMap -> [GameID]
getIDs = reverse . fmap fst . sortOn (time . snd) . M.assocs

nextRound :: Round -> RoundMap -> Maybe Round
nextRound = M.lookup . nextHist

prevRound :: Round -> RoundMap -> Maybe Round
prevRound = M.lookup . prevHist


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
        team       <- res .: "memberResults" >>= withArray "MemberResults" 
                                (traverse (withObject "MemberRes" (\o -> (o .: "player") >>= getName)))

        pWeapons   <- myRes .: "weapons" >>= withArray "my weapons" (traverse (withObject "weapon" getName))
        aWeapons   <- res .: "weapons" >>= withArray "weapons" (traverse (withObject "weapon" getName))

        special    <- myRes .: "specialWeapon" >>= getName
        eggs       <- myRes .: "goldenDeliverCount"
        eggAssists <- myRes .: "goldenAssistCount"
        rescues    <- myRes .: "rescueCount"
        deaths     <- myRes .: "rescuedCount"

        waves      <- res .: "waveResults" >>= traverse parseNJSON :: Parser (Vector WaveStats)

        bosses     <- res .: "enemyResults" >>= V.foldl
                    (\m val -> withObject "bossStats" 
                        (\statObj -> 
                            bossMapInsertStats
                            -- complex map to turn bosses into the Haskell data counterparts
                            -- translating the string representations
                            <$> (statObj .: "enemy" >>= getName >>= maybe (fail "Unknown Boss") pure . textToBoss
                                    :: Parser Boss) 
                            <*> (parseNJSON val :: Parser BossStats)
                            <*> (m :: Parser BossMap))
                        val)
                    (pure bossMapEmpty) :: Parser BossMap

        -- we either parse the king or just give nothing back since it wasn't present
        king <- (fmap Just (parseNJSON (Object res)) <|> pure Nothing)

        next <- res .: "nextHistoryDetail" >>= (.: "id")
        prev <- res .: "previousHistoryDetail" >>= (.: "id")

        -- man, this is an object       
        pure $ CR gameID time stage hazard 
                  username team 
                  pWeapons aWeapons special 
                  eggs eggAssists 
                  rescues deaths 
                  waves 
                  bosses king
                  next prev

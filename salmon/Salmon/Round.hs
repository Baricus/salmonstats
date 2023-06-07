module Salmon.Round (
    -- All statistics about a single round of Salmon Run
    Round(..),
    GameResult(..),
    GameID, RoundMap,
    -- Functions for managing round maps
    toIDMap, getIDs, nextRound, prevRound,
    -- Helper functions for working with Rounds
    isTeammate,
    ) where

import Debug.Trace

import Salmon.NintendoJSON
import Textworthy

import Salmon.Boss
import Salmon.StatMap (StatMap(..))
import Salmon.King
import Salmon.Wave

import Data.Text (Text)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as M

import GHC.Generics ( Generic )
import GHC.Natural ( Natural ) 

import Control.Applicative ( Alternative((<|>)), liftA) 

import Data.Time ( UTCTime )

import Data.Aeson
import Data.Aeson.Types ( Parser )
import Data.List (sortOn)

type GameID = Text
type RoundMap = Map GameID Round

data GameResult = Won -- beat all 3-4 waves
         | Loss Natural -- final wave number
    deriving (Show, Generic)

instance ToJSON GameResult where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GameResult where

data Round = CR
               {
                   gameID        :: Text
                 , time          :: UTCTime
                 , stage         :: Text
                 , result        :: GameResult
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
                 , bosses        :: StatMap Boss BossStats Natural -- not all bosses are present always
                 , king          :: StatMap King KingStats Natural -- singleton or empty map
                 , nextHist      :: Maybe GameID -- not always present, but useful when they are
                 , prevHist      :: Maybe GameID
               }
            deriving (Show, Generic)

-- map of IDs to rounds
toIDMap :: [Round] -> RoundMap
toIDMap rounds = M.fromList [(gameID x, x) | x <- rounds]

-- get's IDs in time order
getIDs :: RoundMap -> [GameID]
getIDs = reverse . fmap fst . sortOn (time . snd) . M.assocs

nextRound :: RoundMap -> Round -> Maybe Round
nextRound m r = nextHist r >>= flip M.lookup m

prevRound :: RoundMap -> Round -> Maybe Round
prevRound m r = prevHist r >>= flip M.lookup m


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
        gameResult <- (\i -> if i == 0 then Won else Loss i) <$> res .: "resultWave"
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
                            -- complex transform to turn bosses into the Haskell data counterparts
                            -- translating the string representations
                            M.insert
                            <$> (statObj .: "enemy" >>= getName >>= maybe (fail "Unknown Boss") pure . fromText :: Parser Boss) 
                            <*> (parseNJSON val :: Parser (BossStats Natural))
                            <*> m)
                        val)
                    (pure M.empty) :: Parser (Map Boss (BossStats Natural))

        -- we either parse the king or just give nothing back since it wasn't present
        king <- ((liftA (uncurry M.singleton) (parseNJSON (Object res))) <|> pure M.empty)

        next <- res .:? "nextHistoryDetail" >>= traverse (.: "id")
        prev <- res .:? "previousHistoryDetail" >>= traverse (.: "id")

        -- man, this is an object       
        pure $ CR gameID time stage gameResult hazard 
                  username team 
                  pWeapons aWeapons special 
                  eggs eggAssists 
                  rescues deaths 
                  waves 
                  (StatMap bosses) (StatMap king)
                  next prev

-- helper/util functions
isTeammate :: Text -> Round -> Bool
isTeammate name = V.elem name . team

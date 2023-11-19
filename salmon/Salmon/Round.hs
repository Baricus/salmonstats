module Salmon.Round (
    -- All statistics about a single round of Salmon Run
    Round(..),
    GameResult(..),
    RoundMap,
    -- Functions for managing round maps
    toIDMap, getIDs, nextRound, prevRound,
    -- functions to parse nxapi data into Rounds objects
    readRoundFromNintendoFile,
    readRoundsFromNXAPIdir,
    -- join in extra shift data
    addShiftData,
    -- Helper functions for working with Rounds
    isTeammate,
    -- Helper functions for working with Game Results
    isWin, isLoss, isDisconnect, isUnknown,
    getWave,
    -- Helper functions for working on round maps
    canonicalizePlayers,
    ) where

import Salmon.NintendoJSON
import Textworthy

import Salmon.Boss
import Salmon.StatMap (StatMap(..))
import Salmon.King
import Salmon.Wave

import Data.Text (Text)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Merge.Strict as M

import GHC.Generics ( Generic )
import GHC.Natural ( Natural ) 

import Control.Applicative ( Alternative((<|>)) ) 

import Data.Time ( UTCTime )

import Data.Aeson
import Data.Aeson.Types ( Parser )

import System.Directory ( listDirectory )
import Data.List (isPrefixOf, sortOn)

import Salmon.Shift ( Shift, GameID ) 
import Salmon.Player
import GHC.Base (join)

type RoundMap = Map GameID Round

data GameResult 
         = Won -- beat all 3-4 waves
         | Loss Natural -- final wave number
         | Disconnect
         | Unknown
    deriving (Show, Generic, Eq)

instance ToJSON GameResult where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GameResult where

data Round = CR
               {
                   gameID        :: Text
                 , time          :: UTCTime
                 , stage         :: Text
                 , shift         :: Maybe Shift
                 , result        :: GameResult
                 , hazard        :: Double
                 , player        :: Player
                 , team          :: Set Player
                 , playedWeapons :: Vector Text
                 , allWeapons    :: Vector Text
                 , special       :: Maybe Text
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
        gameResult <- (\case (i :: Int) | i == (-1) -> Disconnect -- -1 is used for disconnects
                                        | i == 0    -> Won
                                        | otherwise -> Loss . fromIntegral $ i) <$> res .: "resultWave"
        
        hazard     <- res .: "dangerRate"
        username   <- myRes .: "player" >>= parseNJSON
        team       <- res .: "memberResults" >>= withArray "MemberResults" 
                                (traverse (withObject "MemberRes" (\o -> (o .: "player") >>= parseNJSON)))

        pWeapons   <- myRes .: "weapons" >>= withArray "my weapons" (traverse (withObject "weapon" getName))
        aWeapons   <- res .: "weapons" >>= withArray "weapons" (traverse (withObject "weapon" getName))

        special    <- (myRes .: "specialWeapon" >>= fmap Just . getName) <|> pure Nothing
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
        king <- fmap (uncurry M.singleton) (parseNJSON (Object res)) <|> pure M.empty

        next <- res .:? "nextHistoryDetail" >>= traverse (.: "id")
        prev <- res .:? "previousHistoryDetail" >>= traverse (.: "id")

        -- man, this is an object       
        pure $ CR gameID time stage Nothing -- shift data is grabbed from other files and combined
                  gameResult hazard 
                  username (S.fromList . V.toList $ team)
                  pWeapons aWeapons special 
                  eggs eggAssists 
                  rescues deaths 
                  waves 
                  (StatMap bosses) (StatMap king)
                  next prev

-- direct parser functions
readRoundFromNintendoFile :: FilePath -> IO (Either String Round)
readRoundFromNintendoFile = readNintendoJSONFile

readRoundsFromNXAPIdir :: FilePath -> IO [Either String Round]
readRoundsFromNXAPIdir folder = do
    paths <- listDirectory folder
    -- only grab the files that are coop results
    let filtered  = filter (isPrefixOf "splatnet3-coop-result-u") paths
        fullpaths = ((folder <> "/") <>) <$> filtered
    -- read all the files in that folder
    mapM readRoundFromNintendoFile fullpaths

-- combine a GameID -> Shift map and a GameID -> Round Map
-- We drop any extra keys in the shift map, but keep any in the round map.
-- Thus, some Rounds may not have shifts, but only if we didn't have the data for them
addShiftData :: Map GameID Shift -> Map GameID Round -> Map GameID Round
addShiftData = M.merge M.dropMissing M.preserveMissing' $ M.zipWithMatched (\_ s r -> r {shift=Just s})

-- helper/util functions
isTeammate :: Text -> Round -> Bool
isTeammate nameOrNinId = any (isPlayer nameOrNinId) . team

isWin :: GameResult -> Bool
isWin Won = True
isWin _   = False

isLoss :: GameResult -> Bool
isLoss (Loss _) = True
isLoss _        = False

isDisconnect :: GameResult -> Bool
isDisconnect Disconnect = True
isDisconnect _          = False

isUnknown :: GameResult -> Bool
isUnknown Unknown = True
isUnknown _       = False

getWave :: GameResult -> Maybe Natural
getWave Won      = Just 3
getWave (Loss n) = Just n
getWave _        = Nothing

getAllPlayers :: RoundMap -> Map Text Player
getAllPlayers rMap = let rounds = M.elems rMap 
                         teamPlayers = join . fmap (S.elems . team) $ rounds
                         statPlayers = player <$> rounds
                         allPlayers  = teamPlayers <> statPlayers
                         in collapsePlayers allPlayers

-- | Returns a player from the (assumed total) map of players via its ID
-- The map contains players with their proper names
getPlayer :: Map Text a -> Player -> a
getPlayer pMap (Player _ pId) = let fullPlayer = M.lookup pId pMap
                               in case fullPlayer of
                                       (Just newPlayer) -> newPlayer
                                       (Nothing)        -> error "player not found"

givePlayerFullNames :: Functor f => Map Text Player -> f Round -> f Round
givePlayerFullNames pMap = fmap (\r@(CR {player=p}) -> r {player = getPlayer pMap p} )

giveTeamFullNames :: Functor f => Map Text Player -> f Round -> f Round
giveTeamFullNames pMap = fmap (\r@(CR {team=t}) -> r {team=swapTeam t})
    where swapTeam = S.fromList . fmap (getPlayer pMap) . S.toList

canonicalizePlayers :: RoundMap -> RoundMap
canonicalizePlayers rm = let pMap = getAllPlayers rm
                             in giveTeamFullNames pMap . givePlayerFullNames pMap $ rm

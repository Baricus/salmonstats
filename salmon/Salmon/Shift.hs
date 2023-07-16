module Salmon.Shift
    (
        Shift (..),
        GameID,
        Rule,
        StartTime,
        EndTime,
        toIDShiftMap,
        readShiftsFromNXAPIdir,
        readShiftsFromNintendoFile,
    ) where

import Salmon.NintendoJSON

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map.Strict as M

import GHC.Generics ( Generic )
import Data.Time ( UTCTime )

import Data.Aeson
import Data.Aeson.Types (Parser)

import System.Directory ( listDirectory )
import Data.List (isPrefixOf)

--type Mode = Text
type Rule = Text
type StartTime = UTCTime
type EndTime = UTCTime
type GameID = Text

data Shift = PrivateScenario Rule 
           | Regular Rule  StartTime EndTime
           | Limited Rule StartTime EndTime   -- limited availability (eggstra work so far?)
        deriving (Show, Eq, Ord, Generic)

instance ToJSON Shift where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Shift where


instance FromNintendoJSON (Map GameID Shift) where
    parseNJSON = withObject "node" $ \n -> 
            let i = (n .: "mode" :: Parser Text) >>= \case
                    "REGULAR"          -> 
                        Regular <$> (n .: "rule") <*> (n .: "startTime") <*> (n .: "endTime")
                    "PRIVATE_SCENARIO" -> 
                        PrivateScenario <$> (n .: "rule") 
                    "LIMITED"          ->
                        Limited <$> (n .: "rule") <*> (n .: "startTime") <*> (n .: "endTime")
                    mode               -> 
                        fail ("Unknown mode: " <> T.unpack mode)
                m = ((n .: "historyDetails") >>= (.: "nodes") >>= traverse (withObject "round" (.: "id")))
            in M.fromList <$> liftA2 zip m (repeat <$> i)

-- parse every shift in the summary file
instance FromNintendoJSON [Map GameID Shift] where
    parseNJSON = withObject "summary" $ \obj -> (
        (obj .: "result") >>= (.: "historyGroups") >>= (.: "nodes") >>=
            traverse (parseNJSON))

readShiftsFromNintendoFile :: FilePath -> IO (Either String [Map GameID Shift])
readShiftsFromNintendoFile = readNintendoJSONFile

readShiftsFromNXAPIdir :: FilePath -> IO [Either String [Map GameID Shift]]
readShiftsFromNXAPIdir folder = do
    paths <- listDirectory folder
    -- only grab the files that are coop results
    let filtered  = filter (isPrefixOf "splatnet3-coop-summary-") paths
        fullpaths = ((folder <> "/") <>) <$> filtered
    -- read all the files in that folder
    mapM readShiftsFromNintendoFile fullpaths

toIDShiftMap :: [[Map GameID Shift]] -> Map GameID Shift
toIDShiftMap = M.unions . concat

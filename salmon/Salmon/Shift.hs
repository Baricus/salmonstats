module Salmon.Shift where

import Salmon.NintendoJSON
import Salmon.Round (GameID)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Set (Set)
import qualified Data.Set as S

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

data ShiftData = PrivateScenario Rule 
               | Regular Rule  StartTime EndTime
        deriving (Show, Eq, Ord, Generic)

data Shift = Shift
                { matches   :: Set GameID
                , shiftInfo :: ShiftData
                }
        deriving (Show, Eq, Ord, Generic)



instance FromNintendoJSON Shift where
    parseNJSON = withObject "node" $ \n -> 
            let i = (n .: "mode" :: Parser Text) >>= \case
                    "REGULAR"          -> 
                        Regular <$> (n .: "rule") <*> (n .: "startTime") <*> (n .: "endTime")
                    "PRIVATE_SCENARIO" -> 
                        PrivateScenario <$> (n .: "rule") 
                    mode               -> 
                        fail ("Unknown mode: " <> T.unpack mode)
                m = S.fromList <$> ((n .: "historyDetails") >>= (.: "nodes") >>= traverse (withObject "round" (.: "id")))
            in Shift <$> m <*> i

--        Shft
 --       <$> (n .: "startTime")
   --     <*> (n .: "endTime")
     --   <*> (n .: "rule")
       -- <*> (S.fromList <$> ((n .: "historyDetails") >>= (.: "nodes") >>= traverse (withObject "round" (.: "id"))))

-- parse every shift in the summary file
instance FromNintendoJSON (Set Shift) where
    parseNJSON = withObject "summary" $ \obj -> S.fromList <$> (
        (obj .: "result") >>= (.: "historyGroups") >>= (.: "nodes") >>=
            traverse (parseNJSON))

readShiftsFromNintendoFile :: FilePath -> IO (Either String (Set Shift))
readShiftsFromNintendoFile = readNintendoJSONFile

readShiftsFromNXAPIdir :: FilePath -> IO [Either String (Set Shift)]
readShiftsFromNXAPIdir folder = do
    paths <- listDirectory folder
    -- only grab the files that are coop results
    let filtered  = filter (isPrefixOf "splatnet3-coop-summary-") paths
        fullpaths = ((folder <> "/") <>) <$> filtered
    -- read all the files in that folder
    mapM readShiftsFromNintendoFile fullpaths

toIDShiftMap :: Set Shift -> Map Text Shift
toIDShiftMap = foldr (\s m -> foldr (flip M.insert $ s) m (matches s)) M.empty

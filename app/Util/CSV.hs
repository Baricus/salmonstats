module Util.CSV (toCSV) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)

import Salmon.StatMap (StatMap)
import qualified Salmon.StatMap as SM

import Salmon

-- Takes in a function of the key type to a list of fields
-- a function for printing the stat objects found in the map
-- a number for the number of fields to place for empty matches
-- 
-- and the set of keys to match on alongside the ID -> StatMap map
toCSV :: (Textworthy k, Show a, Ord k) => (k -> [Text]) -> (s a -> [Text]) -> Int -> Set k -> Map GameID (StatMap k s a) -> [Text]
toCSV fieldF piecesF piecesCount selSet idMap = 
            -- add in the end of line comma to all lines
            fmap (<> ",") $
            -- build up the list of lines
            csvHeader fieldF selSet : csvLines maybePiecesF selSet idMap
    where maybePiecesF = \cases
                    (Just sm) -> piecesF sm
                    Nothing -> replicate piecesCount ""

csvHeader :: (Textworthy b) => (b -> [Text]) -> Set b -> Text
csvHeader fieldF = T.intercalate "\t" . ("Game ID" :) 
                    . foldr (\boss headLine -> buildRows boss : headLine) []
    where buildRows b = let name = (toText b :: Text)
                            in T.intercalate ",\t" $ name : (fieldF b)

csvLines :: (Show a, Ord k) => (Maybe (s a) -> [Text]) -> Set k -> Map GameID (StatMap k s a) -> [Text]
csvLines toTextPieces selSet = M.foldrWithKey (\i stm csvlines -> buildLine i stm : csvlines) []
    where buildLine i stm = ("," <>) . T.intercalate ",\t" -- adds tabs between data and a comma at the end
            -- adds ID before anything else
            . (i :)
            -- builds string per selected boss
            . foldr (\b l -> (toTextPieces . flip SM.getStats stm) b <> l) [] 
            $ selSet

module Salmon.Player
    (
        Player (..),
        printPlayer,
        collapsePlayers,
        isPlayer,
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.List

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import GHC.Generics ( Generic )

import Data.Aeson
import Salmon.NintendoJSON
    ( FromNintendoJSON(..), getNIdEndSegment )

data Player = Player
            { names :: Set Text
            , npln :: Text
            }
        deriving (Show, Generic)

-- equal and ord should only use the player ID, as players can go by many names
-- and I can't guarentee that every player object will have every name
instance Eq Player where
    (Player _ i1) == (Player _ i2) = i1 == i2

instance Ord Player where
    compare (Player _ i1) (Player _ i2) = compare i1 i2

instance ToJSON Player where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Player where

instance FromNintendoJSON Player where
    parseNJSON = withObject "player" $ \o -> do
        username <- S.singleton <$> o .: "name"
        -- the ID is a base 64 encoded string that decodes into 3 peices, e.g.:
        -- CoopPlayer-u-abvmahs3npgnnum54nmm:20230520T183905_4de71773-f12b-4615-9ad8-165ba07beacc:u-qby4wd4vm44hxiktvnmm
        -- The first piece is ... who knows other than the main tag
        -- the middle is a timestamp
        -- the third is the npln id which is player-unique
        npln <- getNIdEndSegment <$> o .: "id"

        pure $ Player username npln

isPlayerName :: Player -> Text -> Bool
isPlayerName (Player names _) = flip S.member names

isPlayerID :: Player -> Text -> Bool
isPlayerID (Player _ nId) = (== nId)

-- | Returns true if a player has the given ID or name
--
-- NOTE: This may eventually change if we treat IDs as a unique type
isPlayer :: Text -> Player -> Bool
isPlayer t = liftA2 (||) (`isPlayerName` t) (`isPlayerID` t)

printPlayer :: Player -> Text
printPlayer (Player names pId) = pId <> ": " <> (T.intercalate ", " . S.toList $ names)

collapsePlayers :: Foldable f => f Player -> Map Text Player
collapsePlayers = foldl' collapsePlayer M.empty
    where collapsePlayer m p@(Player newNames pId) = M.alter combinePlayerNames pId m
              where 
                  -- If we've already seen this player, combine all the seen names
                  -- If not, add it to the map
                  combinePlayerNames :: Maybe Player -> Maybe Player
                  combinePlayerNames (Just (Player names _)) = Just $ Player (S.union names newNames) pId
                  combinePlayerNames Nothing                 = Just p

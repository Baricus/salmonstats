module Salmon.King where

import Salmon.NintendoJSON

import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics ( Generic )
import GHC.Natural ( Natural ) 

import Data.Aeson
import Data.String (IsString (fromString))
import Textworthy

-- King salmonoids!
data King = Cohozuna
          | Horrorboros
        deriving (Read, Show, Eq, Ord, Bounded, Enum, Generic)

instance ToJSON King where
    toEncoding = genericToEncoding defaultOptions

instance ToJSONKey King where

instance FromJSON King where

instance FromJSONKey King where


instance Textworthy King where
    toText :: IsString s => King -> s
    toText = fromString . show

    fromText :: Text -> Maybe King
    fromText = read . T.unpack


data KingStats a = Kstat
            { killed :: a
            , bronze :: a
            , silver :: a
            , gold   :: a
            }
        deriving (Show, Generic)

instance ToJSON a => ToJSON (KingStats a) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (KingStats a) where

-- parses this from the root object so we can grab the two relevant pieces
--
-- This is a bit weird, but it alowws us to combine the two pieces of data
-- (boss and scales) into one object that either exists or doesn't
-- rather than two
instance FromNintendoJSON (King, KingStats Natural) where
    parseNJSON = withObject "result" $ \obj -> do
        -- these are optional in the Nintendo JSON, but we parse it anyways
        bossRes <- obj .: "bossResult"
        scales  <- obj .: "scale"

        name <- bossRes .: "boss" >>= (.: "name")

        -- defeat is usually a boolean but we want a 0-1 number for consistency
        defeatB <- bossRes .: "hasDefeatBoss"
        let defeat = if defeatB then 1 else 0

        stats <- Kstat defeat <$> scales  .: "bronze"
                              <*> scales  .: "silver"
                              <*> scales  .: "gold"

        -- construct our final object
        pure (name, stats)

instance Functor KingStats where
    fmap :: (a -> b) -> KingStats a -> KingStats b
    fmap f Kstat{killed=k, bronze=b, silver=s, gold=g} = Kstat (f k) (f b) (f s) (f g)

instance Applicative KingStats where
    pure v = Kstat v v v v
    liftA2 f (Kstat k1 b1 s1 g1) (Kstat k2 b2 s2 g2) = Kstat (f k1 k2) (f b1 b2) (f s1 s2) (f g1 g2)

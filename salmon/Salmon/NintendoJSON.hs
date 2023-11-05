module Salmon.NintendoJSON 
    (
        -- A FromJSON mirror for parsing JSON from Nintendo,
        -- as we usually want to discard a significant portion if it.
        -- 
        -- This and the relevant functions allow for defining this custom,
        -- lossy parse, without overwriting the default FromJSON instance.
        FromNintendoJSON (parseNJSON),

        -- Decode a bytestring into a haskell datatype
        decodeNintendoJSON,
        -- Decode a file into a haskell datatype
        readNintendoJSONFile,

        -- Utility functions for dealing with nintendo JSON
        getNIdEndSegment,
    ) where


import Data.Aeson
import Data.Aeson.Types ( parseEither, Parser )

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Base64

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- a class for parsing nintendo JSON
class FromNintendoJSON a where
    parseNJSON :: Value -> Parser a

decodeNintendoJSON :: FromNintendoJSON a => ByteString -> Either String a
decodeNintendoJSON = (>>= parseEither parseNJSON) . eitherDecodeStrict

readNintendoJSONFile :: FromNintendoJSON a => FilePath -> IO (Either String a)
readNintendoJSONFile f =  decodeNintendoJSON <$> B.readFile f

-- | nintendo ID fields are base 64 encoded strings that decodes into 3 peices, e.g.:
-- CoopPlayer-u-abvmahs3npgnnum54nmm:20230520T183905_4de71773-f12b-4615-9ad8-165ba07beacc:u-qby4wd4vm44hxiktvnmm
-- The first piece is ... who knows other than the main tag
-- the middle is a timestamp
-- the third is usually unique per-object.
--
-- In some cases, we want to use the decoded 3rd piece rather than the full, ID (mainly players)
--
-- This function splits off that chunk in decoding
getNIdEndSegment :: Text -> Text
getNIdEndSegment = snd . T.breakOnEnd ":" . either (error . T.unpack) id . decodeBase64

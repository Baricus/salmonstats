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
        readNintendoJSONFile        
    ) where


import Data.Aeson
import Data.Aeson.Types ( parseEither, Parser )

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- a class for parsing nintendo JSON
class FromNintendoJSON a where
    parseNJSON :: Value -> Parser a

decodeNintendoJSON :: FromNintendoJSON a => ByteString -> Either String a
decodeNintendoJSON = (>>= (parseEither parseNJSON)) . eitherDecodeStrict

readNintendoJSONFile :: FromNintendoJSON a => FilePath -> IO (Either String a)
readNintendoJSONFile f = B.readFile f >>= pure . decodeNintendoJSON

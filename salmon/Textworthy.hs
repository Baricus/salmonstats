module Textworthy where

import Data.Text (Text)
import Data.String (IsString, fromString)

class Textworthy a where
    toText   :: IsString s => a -> s
    fromText :: Text -> Maybe a

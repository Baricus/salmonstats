module Textworthy where

import Data.Text (Text)
import Data.String (IsString)

class Textworthy a where
    toText   :: IsString s => a -> s
    fromText :: Text -> Maybe a

module Command (
    -- A wrapper around command to add information and a helper field
    buildCommand
    ) where

import Options.Applicative

buildCommand :: String -> Parser a -> String -> Mod CommandFields a
buildCommand name comm description = command name (info (helper <*> comm) (progDesc description))

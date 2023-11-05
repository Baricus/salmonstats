module Player 
    (
    parseCommand
    ) where

import Options.Applicative

import Salmon

import qualified Data.Map as M

import qualified Data.Set as S

import Data.Text (Text)

data Flag = List
          deriving (Show, Eq, Ord, Bounded, Enum)

parseCommand :: Parser (Round -> RoundMap -> [Text])
parseCommand = handle <$> parser

parser :: Parser Flag
parser = flag' List (long "list" <> short 'l' <> help "List all players")


handle :: Flag -> Round -> RoundMap -> [Text]
handle f _ = case f of
                    List -> fmap printPlayer . S.elems . S.unions . M.elems . fmap team


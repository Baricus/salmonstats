module King (
    parseCommand
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map as M

import Options.Applicative

import Text.Read (readMaybe)

import Salmon
import Salmon.StatMap (StatMap)
import qualified Salmon.StatMap as SM

import Util.StatMap
import Util.CSV
import Config (Config)

data Flag = CSV
          | Sum
          | WinRate
        deriving (Show, Eq, Ord, Bounded, Enum)

data Data = KData (Set King) Flag
    deriving (Show)

parseCommand :: Parser (RoundMap -> [Text])
parseCommand = handle <$> parser

parser :: Parser Data
parser = KData . S.fromList 
        <$> many
                (argument (maybeReader readMaybe) (metavar "KING_NAMES..." <> help "A list of kings to filter for"))
        <*> 
            ( flag' CSV  (long "csv" <> short 'c' <> help "Output boss kills in CSV format on stdout")
          <|> flag' Sum  (long "sum" <> short 's' <> help "Sum boss stats per boss to display")
          <|> flag' WinRate (long "winrate" <> short 'w' <> help "win rate against selected bosses")
        )

-- currently, this just ignores the config
handle :: Data -> RoundMap -> [Text]
handle (KData selected flg) m =
    let selectedSet'   = if S.null selected then S.fromList [minBound..] else selected -- empty = all kings
        -- a map of only the kings we want to display
        -- removing any null maps since most will be that and then we can skip them
        selectedKings  = M.filter (not . SM.null) . M.map ((SM.restrictKeys selectedSet') . king) $ m
    in
        case flg of
             Sum     -> prettyPrintKingMap . statMapsSum $ selectedKings
             WinRate -> prettyPrintWinRate . fmap (\l -> fromIntegral (sum l) / fromIntegral (length l)) . (SM.toMap $ killed) . SM.toStatsList $ selectedKings 
             CSV     -> toCSV kHeader linePieces 4 selectedSet' selectedKings

-- pieces for CSV
kHeader :: Textworthy a => a -> [Text]
kHeader k = fmap ((toText k <> " ") <>) ["killed", "bronze scales", "silver scales", "gold scales"]

linePieces :: Show a => KingStats a -> [Text]
linePieces (Kstat killed bronze silver gold) = fmap textShow [killed, bronze, silver, gold]
        where textShow = T.pack . show

-- pretty printing
prettyPrintKingMap :: Show a => StatMap King KingStats a -> [Text]
prettyPrintKingMap = SM.foldMapWithKey buildLine
    where buildLine king (Kstat {killed=k, bronze=b, silver=s, gold=g}) = 
            [   toText king
            ,   "\t kills: " <> packShow k
            ,   "\tBronze: " <> packShow b
            ,   "\tSilver: " <> packShow s
            ,   "\t  gold: " <> packShow g
            ,   "" -- empty line
            ]

prettyPrintWinRate :: M.Map King Double -> [Text]
prettyPrintWinRate = M.foldMapWithKey buildLine
    where buildLine king avg =
              [ toText king
              , "\tWinrate: " <> packShow avg
              , ""
              ]

packShow :: (Show a) => a -> Text
packShow = T.pack . show

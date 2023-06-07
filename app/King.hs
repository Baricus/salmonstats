module King where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map as M

import Options.Applicative

import Text.Read (readMaybe)

import Salmon
import Salmon.StatMap (StatMap)
import qualified Salmon.StatMap as SM

import Util.StatMap

data Flag = CSV
          | Sum
          | WinRate
        deriving (Show, Eq, Ord, Bounded, Enum)

data Data = KData (Set King) Flag
    deriving (Show)

command :: Parser Data
command = KData . S.fromList 
        <$> many
                (argument (maybeReader readMaybe) (metavar "KING_NAMES..." <> help "A list of kings to filter for"))
        <*> 
            ( flag' CSV  (long "csv" <> short 'c' <> help "Output boss kills in CSV format on stdout")
          <|> flag' Sum  (long "sum" <> short 's' <> help "Sum boss stats per boss to display")
          <|> flag' WinRate (long "winrate" <> short 'w' <> help "win rate against selected bosses")
        )

handle :: Data -> RoundMap -> [Text]
handle (KData selected flg) m =
    let selectedSet'   = if S.null selected then S.fromList [minBound..] else selected -- empty = all kings
        -- a map of only the kings we want to display
        -- removing any null maps since most will be that and then we can skip them
        selectedKings  = M.filter (not . SM.null) . M.map ((`SM.restrictKeys` selectedSet') . king) $ m
    in
        case flg of
             Sum  -> prettyPrintKingMap . statMapsSum $ selectedKings
             _ -> undefined

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
          packShow = T.pack . show

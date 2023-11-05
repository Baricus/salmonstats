module Game (
    parseCommand
    ) where

import Options.Applicative

import Salmon

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)

data Flag = Total
          | WinTot
          | LossTot
          | DisconnectTot
          | WinRate
          | LossRate
          | DisconnectRate
          | AvgWave
          deriving (Show, Eq, Ord, Bounded, Enum)


parseCommand :: Parser (Round -> RoundMap -> [Text])
parseCommand = handle <$> parser


parser :: Parser Flag
parser =  flag' Total          (long "total" <> short 't' <> help "Total games played")
      <|> flag' WinTot         (long "wins" <> short 'w' <> help "Total games lost")
      <|> flag' LossTot        (long "losses" <> short 'l' <> help "Total games won")
      <|> flag' DisconnectTot  (long "disconnects" <> short 'd' <> help "Total games with communication errors")
      <|> flag' WinRate        (long "rate" <> short 'r' <> help "Percentage of total games won (won/total)")
      <|> flag' LossRate       (long "loss-rate" <> help "Percentage of total games lost (lost/total)")
      <|> flag' DisconnectRate (long "disconnect-rate" <> help "Percentage of total games with communcation errors (disconnects/total)")
      <|> flag' AvgWave        (long "average-wave" <> short 'a' <> help "Average wave reached")


handle :: Flag -> Round -> RoundMap -> [Text]
handle f _ = case f of
    Total          -> printRes "Total Games" . calcTotal (const True)
    WinTot         -> printRes "Wins" . calcTotal isWin
    LossTot        -> printRes "Losses" . calcTotal isLoss
    DisconnectTot  -> printRes "Disconnects" . calcTotal isDisconnect
    WinRate        -> printRes "Win-rate" . calcRate isWin
    LossRate       -> printRes "Loss-rate" . calcRate isLoss 
    DisconnectRate -> printRes "DisconnectRate" . calcRate isDisconnect
    AvgWave        -> printRes "Average Wave Reached" . calcAvgWave

-- | Computes the total number of games matching the given filter
calcTotal :: (GameResult -> Bool) -> RoundMap -> Int
calcTotal f = length . filter f . getAllResults

-- | Computes the fraction of games matching the given filter
calcRate :: (GameResult -> Bool) -> Map GameID Round -> Double
calcRate f rm = let res = getAllResults rm
                 in fromIntegral (length . filter f $ res) / fromIntegral (length res)

calcAvgWave :: RoundMap -> Double
calcAvgWave rm = let l = length rm
                     w = sum . mapMaybe getWave . getAllResults $ rm
                  in fromIntegral w / fromIntegral l

-- | gets all the wins and losses from the RoundMap
getAllResults :: RoundMap -> [GameResult]
getAllResults = M.foldl' (flip ((:) . result)) []

-- | Prints out a single line result
printRes :: Show a => Text -> a -> [Text]
printRes name val = pure $ name <> ":\t" <> packShow val

packShow :: (Show a) => a -> Text
packShow = T.pack . show

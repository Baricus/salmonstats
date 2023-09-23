module Config.Offsets 
    -- | This exports configuration for offsetting various calculations to provide accurate counts.
    -- This is most important when computing the sum of killed bosses, since this allows for accounting
    -- for boss kills from before the program's recorded history and, thus, tracking achievements.
    where

import Data.Ini.Config

import Textworthy
import Salmon.Boss

import qualified Data.Vector as V

import GHC.Natural (Natural)

import Salmon.Round 

import qualified Salmon.StatMap as SM

import Data.Time.Clock


offsetRound :: SM.StatMap Boss BossStats Natural -> Round
offsetRound offsetMap = CR
    { gameID        = "OFFSETS"
    , time          = UTCTime (toEnum 0) (secondsToDiffTime 0)
    , stage         = "OFFSETS"
    , shift         = Nothing
    , result        = Unknown
    , hazard        = 0
    , player        = "OFFSETS"
    , team          = V.empty
    , playedWeapons = V.empty
    , allWeapons    = V.empty
    , special       = Nothing
    , eggs          = 0
    , eggAssists    = 0
    , rescues       = 0
    , deaths        = 0
    , waves         = V.empty
    , bosses        = offsetMap
    , king          = SM.empty
    , nextHist      = Nothing
    , prevHist      = Nothing
    }

-- | Parses out a configuration section of boss offsets.  
-- For every listed boss, we create a key with that many kills and spawns.
-- I would add something for team kills too, but there's no way to know exactly how many there were since
-- the point of this is that there's no known data.  
offsetConfig :: IniParser Round
offsetConfig = section "BOSS OFFSETS" $ do
    offs <- traverse (\b -> (\o -> (b, BS o 0 o)) <$> fieldDefOf (toText b) number 0) [minBound @Boss .. maxBound]
    let offsetMap = SM.fromList offs
    pure $ offsetRound offsetMap

defaultOffsets :: SM.StatMap Boss BossStats Natural
defaultOffsets = SM.fromList [(b, BS 0 0 0) | b <- [minBound @Boss .. maxBound]] 

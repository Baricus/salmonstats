module Salmon.StatMap (
    -- A map from some key to a stat's object of some type.
    StatMap(..),
    -- functions to map "map" operations to statMap operations
    liftMapToStatMap,
    liftMap2ToStatMap,
    alterStatMap,    
    -- creation and insertion
    empty,
    insertStats,
    -- query
    getStats,
    Salmon.StatMap.null,
    -- folding to a stat object
    statsFoldl',
    statsFoldr,
    -- folding down to a single value
    foldMapWithKey,
    Salmon.StatMap.foldr,
    foldl',
    -- converting a functor of StatMaps into a single statmap containing [a]
    toStatsList,
    -- remove keys from a statmap
    restrictKeys,
    -- filter values from a statmap
    Salmon.StatMap.filter,
    -- list of all (s a) in the statmap
    elems,
    -- list of keys in the statmap
    keys,
    -- kist of (key, (s a)) pairs
    assocs,
    -- take the union of two maps using the given function
    unionWith,
    ) where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Set (Set)

import GHC.Generics ( Generic )

import Data.Aeson
    ( defaultOptions, genericToEncoding, FromJSON, ToJSON(toEncoding), ToJSONKey, FromJSONKey )

-- some functions for boss maps
newtype StatMap k s a = StatMap { unStatMap :: Map k (s a) }
    deriving (Show, Generic)

instance (ToJSONKey k, ToJSON (s a)) => ToJSON (StatMap k s a) where
    toEncoding = genericToEncoding defaultOptions

instance (Ord k, FromJSONKey k, FromJSON (s a)) => FromJSON (StatMap k s a) where

instance (Functor s) => Functor (StatMap k s) where 
    fmap :: (a -> b) -> StatMap k s a -> StatMap k s b
    -- double fmap to map over the internal stats object s
    fmap f = StatMap . (fmap . fmap) f . unStatMap

-- helper to avoid wrapping in StatMap . f . unStatMap
alterStatMap :: (Map k (s a) -> Map k (s b)) -> StatMap k s a -> StatMap k s b
alterStatMap f = StatMap . f . unStatMap

-- similar helper for doing queries
liftMapToStatMap :: ((Map k (s a)) -> b) -> StatMap k s a -> b
liftMapToStatMap f = f . unStatMap

liftMap2ToStatMap :: ((Map k (s a)) -> (Map k (s b)) -> c) -> StatMap k s a -> StatMap k s b -> c
liftMap2ToStatMap f ma mb = f (unStatMap ma) (unStatMap mb)

-- folds down to a single s
statsFoldr :: (Applicative s) => (a -> b -> b) -> b -> StatMap k s a -> s b
statsFoldr f b = liftMapToStatMap $ Prelude.foldr (liftA2 f) (pure b)

statsFoldl' :: (Applicative s) => (a -> b -> a) -> a -> StatMap k s b -> s a
statsFoldl' f a = liftMapToStatMap $ M.foldl' (liftA2 f) (pure a)

-- folds taking in a bossMap
foldr :: (s a -> b -> b) -> b -> StatMap k s a -> b
foldr f b = liftMapToStatMap $ M.foldr f b 

foldl' :: (a -> s b -> a) -> a -> StatMap k s b -> a
foldl' f a = liftMapToStatMap $ M.foldl' f a

foldMapWithKey :: Monoid m => (k -> s a -> m) -> StatMap k s a -> m
foldMapWithKey f = liftMapToStatMap $ M.foldMapWithKey f

-- convert a foldable of maps to a Map of foldables
toStatsList :: (Applicative s, Ord k) =>  (Foldable f, Functor f) => f (StatMap k s a) -> StatMap k s [a]
toStatsList = Prelude.foldr (unionWith (liftA2 (<>))) empty . ((fmap . fmap) $ pure)

-- builder functions
empty :: StatMap k s a
empty = StatMap M.empty

-- insertion
insertStats :: (Ord k) => k -> s a -> StatMap k s a -> StatMap k s a
insertStats b s = alterStatMap $ M.insert b s

-- query
getStats :: (Ord k) => k -> StatMap k s a -> Maybe (s a)
getStats b m = liftMapToStatMap (M.lookup b) m

null :: StatMap k s a -> Bool
null = Prelude.null . unStatMap

-- utility functions
restrictKeys :: (Ord k) => StatMap k s b -> Set k -> StatMap k s b
restrictKeys m s = alterStatMap (`M.restrictKeys` s) m

filter :: (s b -> Bool) -> StatMap k s b -> StatMap k s b
filter = alterStatMap . M.filter

elems :: StatMap k s a -> [s a]
elems = liftMapToStatMap M.elems

keys :: StatMap k s a -> [k]
keys = liftMapToStatMap M.keys

assocs :: StatMap k s a -> [(k, s a)]
assocs = liftMapToStatMap M.assocs

unionWith :: (Ord k) => (s a -> s a -> s a) -> StatMap k s a -> StatMap k s a -> StatMap k s a
unionWith f ml mr = StatMap . (liftMap2ToStatMap . M.unionWith) f ml $ mr

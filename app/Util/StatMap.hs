module Util.StatMap where

import Salmon.StatMap (StatMap)
import qualified Salmon.StatMap as SM

import Data.Map (Map)
import qualified Data.Map as M

import Data.List (sort, foldl')

-- various helper functions to compute statistics on statMaps

-- folds two maps together into a map of total kills
-- anytime we don't see a boss, it's replaced with all 0's
statMapsSum :: (Foldable f, Functor f, Ord k, Applicative s, Num a) => f (StatMap k s a) -> StatMap k s a
statMapsSum = fmap (foldr (+) 0) . SM.toStatsList

-- takes the average of a collection of statMap, per stat attribute
statMapsAvg :: (Foldable f, Functor f, Integral a, Ord k, Applicative s, Fractional b) => f (StatMap k s a) -> StatMap k s b
statMapsAvg m = let len = length m
          in fmap ((/ fromIntegral len) . fromIntegral . foldr (+) 0) . SM.toStatsList $ m

-- computes the median of each attribute of boss stats
statMapsMedian :: (Foldable f, Functor f, Ord k, Applicative s, Ord a, Integral a, Num b, Fractional b) => f (StatMap k s a) -> StatMap k s (Maybe b)
statMapsMedian = fmap (calcMedian . sort) . SM.toStatsList
    where calcMedian :: (Integral a, Ord a, Num b, Fractional b) => [a] -> Maybe b
          calcMedian [] = Nothing
          calcMedian l | odd (length l) = Just . fromIntegral $ l !! (length l `div` 2)
                       | otherwise      = let index = length l `div` 2
                      in Just . (\e -> fromIntegral e / 2) $ foldl' (+) (0) 
                            [l !! index, l !! (index + 1)]
                

-- finds the largest statistic for each boss out of all the bosses
statMapsMax :: (Ord k, Applicative s, Ord a) => Map k' (StatMap k s a) -> StatMap k s a
statMapsMax = M.foldl' (SM.unionWith (liftA2 max)) SM.empty

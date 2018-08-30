module Utility.Statistics
  ( avg
  , average
  , mean
  , range
  , mode
  , percentile
  , clamp
  , between
  ) where

import Data.Function (on)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (sort, sortBy, groupBy, genericLength)
import Control.Monad (join)

average, avg, mean :: Fractional a => [a] -> a
average = mean
avg = mean
mean = uncurry (/) . foldr (\v (c, n) -> (c + v, n + 1)) (0, 0)

range :: (Num a, Ord a) => [a] -> a
range = uncurry subtract . fromMaybe (0, 0) . foldr range' Nothing
  where
    range' v Nothing = Just (v, v)
    range' v (Just (lo, hi)) = Just (min lo v, max hi v)

mode :: Ord a => [a] -> Maybe a
mode =
    join
  . fmap (\vs -> if length vs == 1 then Nothing else listToMaybe vs)
  . listToMaybe
  . sortBy (flip compare `on` length)
  . groupBy (==)
  . sort

-- percentile vs 0.5 = x, where 50% of the values in vs are < x
-- this is probably extremely slow, b/t the sort and the length check
percentile :: Ord a => [a] -> Float -> a
percentile [] _ = error "percentile: empty list"
percentile vs percentage
  | percentage >= 1.0 = error "percentile: marker too large"
  | percentage < 0 = error "percentile: marker too small"
  | otherwise = (!! floor (genericLength vs * percentage)) . sort $ vs

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

between :: Ord a => a -> (a, a) -> Bool
between x (lo, hi) = lo <= x && x <= hi

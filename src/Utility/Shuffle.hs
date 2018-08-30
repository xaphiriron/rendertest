
module Utility.Shuffle
	( rotate
	, shuffle
	, choose
	, swap
	) where

import System.Random

rotate :: Int -> [a] -> [a]
rotate n xs
	| n > ln || n < 0 = rotate (n `mod` ln) xs
	| otherwise = drop n xs ++ take n xs
		where ln = length xs

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle [] g = ([], g)
shuffle xs g = (head xrs : rest, g'')
	where
		(i, g') = randomR (0, length xs - 1) g
		(rest, g'') = shuffle (tail xrs) g'
		xrs = rotate i xs

choose :: RandomGen g => [a] -> g -> (a, g)
choose xs g = (xs !! i, g')
	where
		(i, g') = randomR (0, ln - 1) g
		ln = length xs

swap :: [a] -> Int -> Int -> [a]
swap xs i j = zipWith swapper [0..] xs
	where swapper index x
		| index == i = xs !! j
		| index == j = xs !! i
		| otherwise = x

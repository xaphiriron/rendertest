module Utility.Tree
	( removeEmpties
	, filterT
	, filterI
	) where

import Data.Maybe
import Data.List
import Data.Tree

removeEmpties :: Tree [a] -> Tree [a]
removeEmpties (Node as chs) = Node as $ foldl' foo [] chs
	where
		foo :: [Tree [a]] -> Tree [a] -> [Tree [a]]
		foo prev cur = case cur of
			Node [] [] -> prev
			Node [] chs' -> prev <> (foldl' foo [] chs')
			Node as' chs' -> prev <> [Node as' $ foldl' foo [] chs']

filterT :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
filterT f n@(Node v chs) = if f n
	then Just $ Node v (filterT f `mapMaybe` chs)
	else Nothing

filterI :: (a -> Bool) -> Tree [a] -> Tree [a]
filterI f = go
	where
		go (Node as chs) = Node (filter f as) $ go <$> chs

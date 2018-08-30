module Data.LSystem
	( LSystem(..)
	, dcfLSystem
	, dLSystem
	, cfLSystem

	, turtlePreLSystem

	, evolve
	, evolveM
	) where

import Control.Monad
import Control.Monad.Identity

import Data.Monoid
import Data.Maybe
import Data.Tree

import Data.Turtle

{-
an l-system would have three predicates: `ignore this symbol for purposes of matching prev/next`; `this symbol is a [-equivalent`; `this symbol is a ]-equivalent`. that first predicate would be the difficult-to-manage one, i think? since depending on how many symbols are flagged that it could lead to arbitrarily-long processing times

(see below at `step`)
-}
data LSystem m a = LSystem
	{ pushSymbol :: a -> Bool
	, popSymbol :: a -> Bool
	, ignoreSymbol :: a -> Bool
	, advance :: Maybe a -> a -> Maybe a -> m [a]
	}

-- deterministic context-free l-system
dcfLSystem :: (a -> [a]) -> LSystem Identity a
dcfLSystem f = LSystem (const False) (const False) (const False) $ \_ a _ -> pure $ f a

-- deterministic l-system
dLSystem :: (Maybe a -> a -> Maybe a -> [a]) -> LSystem Identity a
dLSystem f = LSystem (const False) (const False) (const False) $ \pre a post -> pure $ f pre a post

turtlePreLSystem :: (TurtleSymbol a b -> Bool) -> (Maybe (TurtleSymbol a b) -> TurtleSymbol a b -> Maybe (TurtleSymbol a b) -> [TurtleSymbol a b]) -> LSystem Identity (TurtleSymbol a b)
turtlePreLSystem ignore f = LSystem isPush isPop ignore $ \pre a post -> pure $ f pre a post
	where
		isPush s = case s of
			TA Push -> True
			_ -> False
		isPop s = case s of
			TA Pop -> True
			_ -> False


cfLSystem :: (a -> m [a]) -> LSystem m a
cfLSystem f = LSystem (const False) (const False) (const False) $ \_ a _ -> f a

evolve :: LSystem Identity a -> Int -> [a] -> [a]
evolve l i = runIdentity . evolveM l i

evolveM :: Monad m => LSystem m a -> Int -> [a] -> m [a]
evolveM l = go
	where
		go 0 z = pure z
		go i z = go (i-1) =<< step l z

-- note that this doesn't ignore stack push/pops in terms of looking for pre/post symbols
{- the way to handle that would be i think to turn [a] into a Tree [a]:
 - in the absence of other rule application, symbols would be accumulated in a single [a] node.
 - a [ symbol would move parsing down a level, into a new child node
 - a ] symbol would move parsing up and then down a level, into a new sibling node

 - the symbol that is BEFORE a given symbol would be the symbol before it in that node's list, or, if that symbol starts the list, the last node in its parent node's list
 - the symbol that is AFTER a given symbol would be the symbol after it in that node's list, or, if that symbol ends the list, the first node in the last child's node list

a[bc]d[ef]

   a
  / \
 bc  d
    /
   ef

this also might lead to step/evolve internally using a tree representation of the string

parseIntoTree :: LSystem m a -> [a] -> Tree [a]
parseIntoTree (LSystem push pop ignore _) = go (pure [])
	where
		go z [] = z
		go z (s:ss)
			| push s = z { subForest = go (pure []) ss : subForest z }
			| pop s = -- ???
			| otherwise = go (z { rootLabel = s : rootLabel z }) ss
-}


{- this properly sets pred/succ values, based on push/pop/ignore values. specifically:
the succ of a value will be the first non-ignored successor value not within a deeper or shallower stack push
the pred of a value will be the last non-ignored prior value not within a deeper stack push, but potentially within a shallower push

in AB[C]D:
	. < A > B
	A < B > D
	B < C > .
	B < D > .
-}
step :: Monad m => LSystem m a -> [a] -> m [a]
step (LSystem push pop ignore adv) = go []
	where
		-- we push [ onto the prev list, so that we can properly track the depth, but we don't ever want to actually _use_ [ as a prev symbol, so it's always filtered out
		validPrevSymbol = listToMaybe . filter (not . push)
		validPostSymbol [] = Nothing
		validPostSymbol (v:rs)
			| pop v = Nothing -- if we've hit the end of a stack push, there's no following symbol
			| push v = tilMatchingPop 1 rs -- drop until we hit the matching pop (not just `dropWhile (not . pop)` b/c there might be many nested pushes
			| ignore v = validPostSymbol rs
			| otherwise = Just v
		tilMatchingPop 0 rs = validPostSymbol rs
		tilMatchingPop n [] = Nothing
		tilMatchingPop n (v:rs)
			| push v = tilMatchingPop (n+1) rs
			| pop v = tilMatchingPop (n-1) rs
			| otherwise = tilMatchingPop n rs
		go _ [] = pure []
		go prevs (cur:[]) = do
			adv (validPrevSymbol prevs) cur Nothing
		go prevs (cur:rest) = do
			ev <- if pop cur
				then adv (validPrevSymbol prevs) cur Nothing
				else adv (validPrevSymbol prevs) cur (validPostSymbol rest)
			rest <- case (ignore cur, push cur, pop cur) of
				(True, _, _) -> go prevs rest
				(False, True, _) -> go (cur : prevs) rest -- add the push symbol as a placeholder
				(False, False, True) -> go (drop 1 prevs) rest -- remove the in-stack prev
				_ -> go (cur : drop 1 prevs) rest -- replace the old prev symbol with this one
			return $ ev <> rest
{-
step :: Monad m => LSystem m a -> [a] -> m [a]
step (LSystem _ _ ignore adv) = go Nothing
	where
		nextValid [] = Nothing
		nextValid (v:rs)
			| ignore v  = nextValid rs
			| otherwise = Just v
		go _ [] = pure []
		go prev (cur:[]) = do
			adv prev cur Nothing
		go prev (cur:rs) = do
			ev <- adv prev cur (nextValid rs)
			rest <- if ignore cur
				then go prev rs
				else go (Just cur) rs
			return $ ev <> rest
-}
{-
KINDS OF LSYSTEMS:

the basic kind (w/ parametric possiblities)
evolving in a monad (a -> m [a])
prelookup and postlookup (respecting stack motions) -- something like Maybe a -> a -> Maybe a -> [a]

the kind that evolve based on a "condition" on a "map" (e.g., evolve in the direction of light by checking light on the location and growing or withering based on that) (wait is this just a specific case of "in a (state) monad"?)
-}

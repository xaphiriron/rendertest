{-# LANGUAGE ScopedTypeVariables #-}

module Data.LSystem
	( LSystem(..)
	, dcfLSystem
	, dLSystem
	, cfLSystem

	, turtlePreLSystem

	, evolve
	, evolveM


	, FullContextLSystem(..)
	, treeAsString
	, stringAsTree

	, stepTree
	, stepString

	, evolveFull
	, evolveFullM

	) where

import Control.Monad
import Control.Monad.Identity

import Data.Monoid
import Data.Maybe
import Data.Tree
import Data.Tree.Zipper
import Data.List (foldl')

import Data.Turtle
import Utility.Tree

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
to add

* reading environment state (light, soil/air/etc materials)
* writing environment state (nutrients, light, plant material state)

the kind that evolve based on a "condition" on a "map" (e.g., evolve in the direction of light by checking light on the location and growing or withering based on that) (wait is this just a specific case of "in a (state) monad"?)
-}

data FullContextLSystem m a = FCLSystem
	{ _pushSymbol :: a -> Bool -- this symbol starts a 'branch', or the potential for one
	, _popSymbol :: a -> Bool -- this symbol ends a 'branch', and returns the 'turtle' back to the state at the matching push symbol
	, _ignoreSymbol :: a -> Bool -- this symbol is ignored for determining pred/succ context
	, _advance :: [a] -> a -> Tree [a] -> m [a]
	}

evolveFull :: FullContextLSystem Identity a -> Int -> Tree [a] -> Tree [a]
evolveFull l i = runIdentity . evolveFullM l i

evolveFullM :: Monad m => FullContextLSystem m a -> Int -> Tree [a] -> m (Tree [a])
evolveFullM l = go
	where
		go 0 z = pure z
		go i z = go (i - 1) =<< stepTree l z

stepString :: Applicative m => FullContextLSystem m a -> [a] -> m [a]
stepString lsys = fmap treeAsString . stepTree lsys . stringAsTree lsys

-- todo: probably would be more efficient w/ the filter happening somewhere else, but idk where exactly to put it so that ignored symbols get replicated in the next generation regardless. maybe laziness will help regardless
stepTree :: Applicative m => FullContextLSystem m a -> Tree [a] -> m (Tree [a])
stepTree l@(FCLSystem _ _ ignore adv) z = contextMapA l expand z
	where expand pred c succ = if ignore c
		then pure [c]
		else adv (filter (not . ignore) pred) c (filter (not . ignore) <$> succ)

contexts :: Tree [a] -> [([a], a, Tree [a])]
contexts = goT []
	where
		go preds [] _ = []
		go preds (c:succs) chs = (preds, c, Node succs chs) : go (c:preds) succs chs
		goT preds (Node syms chs) = go preds syms chs <> (goT (reverse syms <> preds) =<< chs)

-- todo: this has to reverse `syms` twice, so probably i could cache that (have `go` return a reversed list) and avoid the extra call to `reverse`. or name the go call in goT and use the head of the list to reconstruct the reversed syms.
contextT :: Tree [a] -> Tree [([a], a, Tree [a])]
contextT = goT []
	where
		go preds [] _ = []
		go preds (c:succs) chs = (preds, c, Node succs chs) : go (c:preds) succs chs
		goT preds (Node syms chs) =
			Node (go preds syms chs) (goT (reverse syms <> preds) <$> chs)

contextMap :: ([a] -> a -> Tree [a] -> b) -> Tree [a] -> Tree [b]
contextMap f = fmap (fmap $ \(pred, a, succ) -> f pred a succ) . contextT

contextMapL :: ([a] -> a -> Tree [a] -> [b]) -> Tree [a] -> Tree [b]
contextMapL f = fmap ((\(pred, a, succ) -> f pred a succ) =<<) . contextT


{- the 'stringAsTree l . treeAsString' part isn't ideal, but it's pretty tricky to correctly reconstruct each expansion, especially given that l-systems like
	seed: xyy
	x -> [x[
	y -> y]
exist and need to be properly handled
-}
contextMapA :: forall m a b. Applicative m => FullContextLSystem m a -> ([a] -> a -> Tree [a] -> m [a]) -> Tree [a] -> m (Tree [a])
contextMapA l f t = fmap (removeEmpties . stringAsTree l . treeAsString) mExtractTree
	where
		mExtractTree :: m (Tree [a])
		mExtractTree = sequenceA . fmap (fmap concat . sequenceA) $ advancedTree
		advancedTree :: Tree ([m [a]])
		advancedTree = (\ms -> (\(left, pred, right) -> f left pred right) <$> ms) <$> contextTree
		contextTree :: Tree [([a], a, Tree [a])]
		contextTree = contextT t

reunify :: FullContextLSystem m a -> Tree (Tree [a]) -> Tree [a]
reunify l t = stringAsTree l $ treeAsString $ fmap treeAsString $ t

treeAsString :: Tree [a] -> [a]
treeAsString (Node str chs) = str <> (treeAsString =<< chs)

--- this creates a tree from a string, and does not do any further processing (e.g., stripping the push/pop symbols themselves, or deleting ignored nodes)
stringAsTree :: FullContextLSystem m a -> [a] -> Tree [a]
stringAsTree (FCLSystem push pop _ _) =
		fullReverse . toTree . snd
		. foldl handleSymbol ([0], fromTree $ Node [] [])
	where
		handleSymbol (stack, z) a
			| push a = (0 : stack, jumpToNewBranch $ addToNewBranch [a] z)
			| pop a = case stack of
				n:m:rs -> (m + 1 : rs, jumpToNewBranch $ addToNewBranch [] $ stepUp (n+1) (addToCur a z))
				_ -> error $ "unmatched pop (1)"
			| otherwise = (stack, addToCur a z)
		addToCur a = modifyTree (\t -> t {rootLabel = a : rootLabel t })
		addToNewBranch a = modifyTree (\t -> t {subForest = pure a : subForest t })
		jumpToNewBranch = fromMaybe (error "couldn't find new branch") . firstChild
		stepUp n t = case n of
			0 -> t
			n -> stepUp (n - 1) $ fromMaybe (error "unmatched pop (2)") $ parent t
		fullReverse (Node syms forest) = Node (reverse syms) $ fullReverse <$> reverse forest

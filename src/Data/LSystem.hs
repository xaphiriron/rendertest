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
stepTree (FCLSystem _ _ ignore adv) z = contextMapA expand z
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

contextMapA :: Applicative m => ([a] -> a -> Tree [a] -> m [b]) -> Tree [a] -> m (Tree [b])
contextMapA f = sequenceA
	. fmap (\mas -> fmap concat . sequenceA $ (\(pred, a, succ) -> f pred a succ) <$> mas)
	. contextT

treeAsString :: Tree [a] -> [a]
treeAsString (Node str chs) = str <> (treeAsString =<< chs)

-- this creates a tree from a string, and does not do any further processing (e.g., stripping the push/pop symbols themselves, or deleting ignored nodes)
stringAsTree :: FullContextLSystem m a -> [a] -> Tree [a]
stringAsTree (FCLSystem push pop _ _) = fullReverse . toTree . go (fromTree $ pure [])
	where
		go z [] = z
		go z (a:as)
			| push a = go (jumpToNewBranch $ addAsNewBranch [a] z) as
			| pop a = case parent (addSymbolToCur a z) of
				Nothing -> error "unmatched pop"
				{- if there's nothing in the parent node and this branch is the only branch from it, then we have a state like `a[bc][de]` where `[de]` should be considered to be the direct child of `a`, same as `[bc]`. so:
					pull the latest branch up, attach it to its grandparent
					stay focused on the now-childless [] branch
				otherwise
					we need to make a new [] branch
				-}
				Just z' -> if null (label z') && length (subForest $ tree z') == 1 && isContained z'
					then let
							latestBranch = head $ subForest $ tree z'
							cutTree = modifyTree (\t -> t { subForest = [] }) z'
						in case (do
								grandparent <- parent cutTree
								addBeforeGap latestBranch grandparent) of
							Nothing -> error ":("
							Just z' -> go (jumpToNewBranch z') as
					else go (jumpToNewBranch $ addAsNewBranch [] z') as
			| otherwise = go (addSymbolToCur a z) as
		addSymbolToCur a = modifyTree (\t -> t {rootLabel = a : rootLabel t })
		addAsNewBranch a = modifyTree (\t -> t {subForest = pure a : subForest t })
		addBeforeGap :: Tree a -> TreePos Full a -> Maybe (TreePos Full a)
		addBeforeGap a z = do
				gap <- next . first . children $ z
				parent . insert a $ gap
		jumpToNewBranch = fromMaybe (error "couldn't find new branch") . firstChild
		fullReverse (Node syms forest) = Node (reverse syms) $ fullReverse <$> reverse forest

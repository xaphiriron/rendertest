module Generator.Coast
	( Coast(..)
	, CoastEdge(..)
	, coastGenerator
	) where

import Data.Graph.Inductive

import Utility.Rand

import Data.GraphGrammar
import Generator.Dungeon

data Coast = Water | Coastline Int | RecessedCoastline Int | Tidecave | Inlet | Waterfall
	deriving (Eq, Ord, Show, Read)

data CoastEdge = ToWater | CoastEdge | Other
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

baseCoast :: RandomGen g => (Int -> p) -> Rand g (Gr (GenNode p Coast) (GenEdge CoastEdge))
baseCoast starter = return $
	mkGraph
		[ (0, ND (starter 0) [] 0
			$ Inlet)
		, (1, ND (starter 1) [] 0
			$ Coastline 1)
		, (2, ND (starter 2) [] 0
			$ Water)
		]
		[(0, 1, base Other), (1, 2, base ToWater)]

isCoastline :: Coast -> Bool
isCoastline (Coastline _) = True
isCoastline _ = False

coastlineLonger :: Int -> Coast -> Bool
coastlineLonger x (Coastline y) | x < y = True
coastlineLonger _ _ = False

isRCoastline :: Coast -> Bool
isRCoastline (RecessedCoastline _) = True
isRCoastline _ = False

rcoastlineLonger :: Int -> Coast -> Bool
rcoastlineLonger x (RecessedCoastline y) | x < y = True
rcoastlineLonger _ _ = False

coastGenerator :: (RandomGen g) => (Int -> p)
	-> (Coast -> Rand g p)
	-> Embedding g p Gr (GenNode p Coast) (GenEdge CoastEdge)
	-> Int
	-> Rand g (GraphGrammar g p Gr (GenNode p Coast) (GenEdge CoastEdge))
coastGenerator starter fp embedding n = do
	landscape <- baseCoast starter
	return $ GG
		landscape
		(concat
			[ replicate 4 $ expandLinkedPair
				(isCoastline, (== Water))
				( Mutate 0 $ \c -> case getInfo c of
					Coastline x -> fillBase (getLoc c) $ Coastline (x+1)
					_ -> error "..."
				, Mutate 1 $ \c -> fillBase (getLoc c) Water
				)
			, replicate 4 $ expandLinkedPair
				(isRCoastline, (== Water))
				(Mutate 0 $ \c -> case getInfo c of
					RecessedCoastline x -> fillBase (getLoc c) $ RecessedCoastline (x+1)
					_ -> error "..."
				, Mutate 1 $ \c -> fillBase (getLoc c) Water)
			, replicate 3 $ expandLinkedPair
				(rcoastlineLonger 3, (== Water))
				(Mutate 0 $ \c -> fillBase (getLoc c) $ Coastline 1
				,Mutate 1 $ \c -> fillBase (getLoc c) Water
				)
			, replicate 3 $ expandLinkedPair
				(coastlineLonger 3, (== Water))
				(Mutate 0 $ \c -> fillBase (getLoc c) $ RecessedCoastline 1
				,Mutate 1 $ \c -> fillBase (getLoc c) Water
				)
			, replicate 2 $ addLinkingFrom
				[\(c, es) -> rcoastlineLonger 1 c
					&& length (filter (== CoastEdge) es) >= 2]
				Tidecave fp
			, replicate 2 $ addLinkingFrom [\(c, es) -> coastlineLonger 1 c
					&& length (filter (== CoastEdge) es) >= 2]
				Inlet fp
			, pure $ addLinkingFrom [(== Inlet) . fst, (== Waterfall) . fst] Inlet fp
			, pure $ addLinkingFrom [(== Inlet) . fst, (== Waterfall) . fst] Waterfall fp
			, pure $ addLinkingFrom [(== Tidecave) . fst] Tidecave fp
			, replicate 3 $ addBetween isRCoastline isRCoastline [Tidecave] fp
			, replicate 3 $ addBetween isRCoastline isRCoastline [Tidecave, Tidecave] fp
-- these aren't safe to use, because the embedder doesn't recheck modified edges, so the embedding can be broken
--			, replicate 9 $ relinkOne isRCoastline (== Tidecave)
--		, relinkRecessed (isRCoastline) (== Tidecave)
--		, relinkRecessed (== Tidecave) (== Tidecave)
--		, relinkRecessed (isCoastline) (== Inlet)
--		, relinkRecessed (`elem` [Inlet, Waterfall]) (`elem` [Inlet, Waterfall])
			])
		embedding
		(\x -> stopAtSize n x)
		(Just $ flipGenEdge flipId)
		embedGenNode

{-
edgeDeterminer :: Coast -> Coast -> CoastEdge
edgeDeterminer a b
	| a == Water || b == Water = ToWater
	| a `elem` [Coastline, RecessedCoastline] && b `elem` [Coastline, RecessedCoastline] = CoastEdge
	| otherwise = Other
-}

expandLinkedPair :: (RandomGen g, Graph gr, Eq a) =>
	(a -> Bool, a -> Bool) ->
	(Selection (GenNode p a) Node, Selection (GenNode p a) Node) ->
	Rand g (Expansion gr (GenNode p a) (GenEdge CoastEdge))
expandLinkedPair (existsA, existsB) (newA, newB) = do
	-- ap <- undefined -- fp newA
	-- bp <- undefined -- fp newB
	return $ Exp
		(mkGraph
			[ (0, \(r, _) -> existsA $ label r)
			, (1, \(r, _) -> existsB $ label r)
			]
			[ (0, 1, const True)])
		(mkGraph
			[ (0, Copy 0), (1, Copy 1)
			, (2, newA)
			, (3, newB)
			]
			[ (0, 1, Copy (0, 1))
			, (0, 2, Static $ base CoastEdge)
			, (1, 3, Static $ base ToWater)
			, (2, 3, Static $ base ToWater)
			])

{-
addUniquely :: (RandomGen g, Graph gr, Eq a) => a -> a -> (CoastEdge -> Bool) -> Int -> (a -> Rand g p) -> Rand g (Expansion gr (GenNode p a) (GenEdge CoastEdge))
addUniquely from add prohibit prohibitCount fp = do
	p <- fp add
	return $ Exp
		(mkGraph
			[(0, \(r, es) -> length (filter (\(ED _ _ a) -> prohibit a) es) < prohibitCount && label r == from)]
			[])
		(mkGraph
			[(0, Copy 0), (1, Static $ fillBase p add)]
			[(0, 1, Static $ base CoastEdge)])
-}

-- i think there might be a bug in the expander where it doesn't break an edge if it's been matched backwards, or something like that, because it seeems like ~50% of the time that 0, 1 edge remains after this pattern is matched
addBetween :: (RandomGen g, Graph gr, Eq a) => (a -> Bool) -> (a -> Bool) -> [a] -> (a -> Rand g p) -> Rand g (Expansion gr (GenNode p a) (GenEdge CoastEdge))
addBetween from to add fp = do
	ps <- (\a -> Static <$> (fillBase <$> fp a <*> pure a)) `mapM` add
	let newNodes = zip [2..] ps
	let l = length newNodes
	let interEdges = [(n+1, n+2, Static $ base Other) | n <- [1..l-1]]
	let startEndEdges = [(0, 2, Static $ base Other), (l+1, 1, Static $ base Other)]
	return $ Exp
		(mkGraph
			[ (0, \(r, _) -> from (label r))
			, (1, \(r, _) -> to (label r))
			]
			[(0, 1, const True)]
		)
		(mkGraph
			([(0, Copy 0), (1, Copy 1)] ++ newNodes)
			(startEndEdges ++ interEdges)
		)

_relinkOne :: Graph gr => (a -> Bool) -> (a -> Bool) -> Rand g (Expansion gr (GenNode p a) (GenEdge CoastEdge))
_relinkOne original recessedPair = do
	return $ Exp
		(mkGraph
			[ (0, \(r, _) -> original $ label r)
			, (1, \(r, _) -> recessedPair $ label r)
			, (2, \(r, _) -> recessedPair $ label r)
			]
			[ (0, 1, const True)
			, (0, 2, const True)
			])
		(mkGraph
			[ (0, Copy 0), (1, Copy 1), (2, Copy 2) ]
			[ (1, 2, Static $ base Other)
			])

_relinkRecessed :: Graph gr => (a -> Bool) -> (a -> Bool) -> Rand g (Expansion gr (GenNode p a) (GenEdge CoastEdge))
_relinkRecessed originalPair recessedPair = do
	return $ Exp
		(mkGraph
			[ (0, \(r, _) -> originalPair $ label r)
			, (1, \(r, _) -> originalPair $ label r)
			, (2, \(r, _) -> recessedPair $ label r)
			, (3, \(r, _) -> recessedPair $ label r)
			]
			[ (0, 1, const True)
			, (0, 2, const True)
			, (1, 3, const True)
			])
		(mkGraph
			[ (0, Copy 0), (1, Copy 1), (2, Copy 2), (3, Copy 3) ]
			[ (0, 2, Copy (0, 2))
			, (1, 3, Copy (1, 3))
			, (2, 3, Static $ base Other)
			])

addLinkingFrom :: (Functor t, Foldable t, RandomGen g, Graph gr, Eq a) => t ((a, [CoastEdge]) -> Bool) -> a -> (a -> Rand g p) -> Rand g (Expansion gr (GenNode p a) (GenEdge CoastEdge))
addLinkingFrom from add fp = do
	p <- fp add
	return $ Exp
		(mkGraph
			[(0, \(r, es) -> or ((\f -> f (label r, label <$> es)) <$> from))]
			[])
		(mkGraph
			[(0, Copy 0), (1, Static $ fillBase p add)]
			[(0, 1, Static $ base Other)])

fillBase :: p -> n -> GenNode p n
fillBase p n = ND p [] 0 n
{-
fillBaseM :: Applicative m => m p -> n -> m (GenNode p n)
fillBaseM p n = ND <$> p <*> pure [] <*> pure 0 <*> pure n
-}
getLoc ::GenNode p n -> p
getLoc (ND p _ _ _) = p

getInfo :: GenNode p n -> n
getInfo (ND _ _ _ n) = n


module Generator.Landscape where

import Data.GraphGrammar
import Generator.Dungeon

-- import Data.List
import Linear.Vector ((^*))
import Linear.V3

import Utility.Rand
import Utility.Statistics (average)

import Data.Graph.Inductive

data Location = L
	{ landscape :: Landscape
	, shape :: LShape
	, size :: Int
	}
	deriving (Eq, Ord, Show, Read)

data LShape = Blob | XLine | Poly Int
	deriving (Eq, Ord, Show, Read)

data Landscape
	= Swamp
	| Canyon | Coast | Desert | Caverns
	| Mountain | Forest | Lake | River | Hills | Prarie
	| Garrison | Village | Highroad | Junkheap | Wastes | Fields
	| CITY | Cathedral | Church
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

landscapeSize :: RandomGen g => Landscape -> Rand g Int
landscapeSize l =
	let bounds = case l of
		Swamp -> (21, 32)
		Canyon -> (13, 26)
		Coast -> (33, 172)
		Desert -> (13, 21)
		Caverns -> (7, 98)
		Mountain -> (11, 16)
		Forest -> (19, 31)
		Lake -> (7, 41)
		River -> (10, 20)
		Hills -> (5, 9)
		Prarie -> (27, 39)
		Garrison -> (2, 6)
		Village -> (1, 5)
		Highroad -> (9, 12)
		Junkheap -> (2, 7)
		Wastes -> (20, 30)
		Fields -> (15, 26)
		CITY -> (15, 18)
		Cathedral -> (5, 13)
		Church -> (1, 5)
	in liftRand $ randomR bounds

landscapeShape :: Landscape -> LShape
landscapeShape l = case l of
	Mountain -> Poly 7
	CITY -> Poly 18
	Village -> Poly 5
	Garrison -> Poly 5
	Highroad -> XLine
	River -> XLine
	Canyon -> XLine
	_ -> Blob

location :: Landscape -> Int -> Location
location l size = L l (landscapeShape l) size

fillBase :: p -> n -> GenNode p n
fillBase p n = ND p [] 0 n

baseLandscape :: RandomGen g => Rand g (Gr (GenNode (V3 Float) Location) (GenEdge ()))
baseLandscape = do
	angle <- (\x -> x * pi * 2) <$> liftRand (randomR (0, 1 :: Float))
	let dim = V3 (sin angle) (cos angle) 0
	citySize <- landscapeSize CITY
	highroadSize <- landscapeSize Highroad
	swampSize <- landscapeSize Swamp
	mountainSize <- landscapeSize Mountain
	let lineAt = (\x -> dim ^* fromIntegral x) :: Int -> V3 Float
	return $ mkGraph
		[ (0, ND (lineAt $ sum [highroadSize, (citySize + swampSize) `div` 2]) [] 0
			$ location Swamp swampSize)
		, (1, ND (lineAt $ sum [highroadSize, swampSize, (citySize + mountainSize) `div` 2]) [] 0
			$ location Mountain mountainSize)
		, (2, ND (lineAt $ 0) [] 0
			$ location CITY citySize)
		, (3, ND (lineAt $ (citySize + highroadSize) `div` 2) [] 0
			$ location Highroad highroadSize)
		]
		[(0, 1, base ()), (2, 3, base ()), (3, 0, base ())]

baseLandscapeMetric :: RandomGen g => (Int -> p) -> Rand g (Gr (GenNode p Location) (GenEdge ()))
baseLandscapeMetric starter = do
	citySize <- landscapeSize CITY
	{-
	highroadSize <- landscapeSize Highroad
	swampSize <- landscapeSize Swamp
	mountainSize <- landscapeSize Mountain
	cathedralSize <- landscapeSize Cathedral
	-}
	return $ mkGraph
		[{- (0, ND (starter 0) [] 0
			$ location Mountain mountainSize)
		, (1, ND (starter 1) [] 0
			$ location Swamp swampSize)
		, (2, ND (starter 2) [] 0
			$ location Highroad highroadSize)
		,-} (3, ND (starter 3) [] 0
			$ location CITY citySize)
		{-, (4, ND (starter 4) [] 0
			$ location Cathedral cathedralSize)-}
		]
		[{-(0, 1, base ()), (1, 2, base ()), (2, 3, base ()), (3, 4, base ())-}]

roadLink :: (RandomGen g, Graph gr) => Landscape -> (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
roadLink x fp = do
	size <- landscapeSize x
	let loc = location x size
	p <- fp loc
	return $ Exp
		(mkGraph
			[ (0, \(r, _) -> landscape (label r) `elem` [CITY, Highroad])
			, (1, \(r, _) -> landscape (label r) `elem` [CITY, Highroad])
			]
			[(0, 1, const True)])
		(mkGraph
			[(0, Copy 0), (1, Copy 1), (2, Static $ fillBase p loc)]
			[(0, 2, Static $ base ()), (2, 1, Static $ base ())])

roadBranch :: (RandomGen g, Graph gr) => Landscape -> (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
roadBranch l fp = do
	highroadSize <- landscapeSize Highroad
	size <- landscapeSize l
	let highroadLoc = location Highroad highroadSize
	let loc = location l size
	hp <- fp highroadLoc
	p <- fp loc
	return $ Exp
		(mkGraph
			[(0, \(r, _) -> landscape (label r) `elem` [CITY, Village, Garrison, Highroad, Cathedral, Church])]
			[])
		(mkGraph
			[ (0, Copy 0)
			, (1, Static $ fillBase hp highroadLoc)
			, (2, Static $ fillBase p loc)
			]
			[(0, 1, Static $ base ()), (1, 2, Static $ base ())])

expandHighroad :: (RandomGen g, Graph gr) => (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
expandHighroad fp = do
	highroadSize <- landscapeSize Highroad
	let loc = location Highroad highroadSize
	p <- fp loc
	return $ Exp
		(mkGraph
			[ (0, \(r, _) -> landscape (label r) `elem` [CITY, Highroad, Village, Garrison, Cathedral, Church, Swamp])
			, (1, \(r, _) -> landscape (label r) `elem` [CITY, Highroad, Village, Garrison, Cathedral, Church, Swamp])]
			[(0, 1, const True)])
		(mkGraph
			[(0, Copy 0), (1, Copy 1), (2, Static $ fillBase p loc)]
			[(0, 2, Static $ base ()), (2, 1, Static $ base ())])

branchUrban :: (RandomGen g, Graph gr) => Landscape -> (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
branchUrban x fp = do
	size <- landscapeSize x
	let loc = location x size
	p <- fp loc
	return $ Exp
		(mkGraph
			[(0, \(r, _) -> landscape (label r) `elem` [CITY, Highroad, Village, Garrison, Cathedral, Church])]
			[])
		(mkGraph
			[(0, Copy 0), (1, Static $ fillBase p loc)]
			[(0, 1, Static $ base ())])

addWildTri :: (RandomGen g, Graph gr) => Landscape -> (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
addWildTri w fp = do
	size <- landscapeSize w
	let loc = location w size
	p <- fp loc
	return $ Exp
		(mkGraph
			[ (0, \(r, es) -> length es < 4 && not (landscape (label r) `elem` [CITY, Village, Highroad, Garrison, Cathedral, Church]))
			, (1, \(r, es) -> length es < 4 && not (landscape (label r) `elem` [CITY, Village, Highroad, Garrison, Cathedral, Church]))
			]
			[(0, 1, const True)])
		(mkGraph
			[(0, Copy 0), (1, Copy 1), (2, Static $ fillBase p loc)]
			[(0, 1, Copy (0, 1)), (1, 2, Static $ base ()), (0, 2, Static $ base ())])

addWildCoast :: (RandomGen g, Graph gr) => (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
addWildCoast fp = do
	canyonSize <- landscapeSize Canyon
	coastSize <- landscapeSize Coast
	let canyonLoc = location Canyon canyonSize
	let coastLoc = location Coast coastSize
	canyonP <- fp canyonLoc
	coastP <- fp coastLoc
	return $ Exp
		(mkGraph
			[ (0, \(r, es) -> length es < 4 && not (landscape (label r) `elem` [CITY, Village, Highroad, Garrison, Cathedral]))
			, (1, \(r, es) -> length es < 4 && landscape (label r) `elem` [Lake, River])
			]
			[(0, 1, const True)])
		(mkGraph
			[ (0, Copy 0)
			, (1, Copy 1)
			, (2, Static $ fillBase canyonP canyonLoc)
			, (3, Static $ fillBase coastP coastLoc)
			]
			[(0, 1, Copy (0, 1)), (1, 2, Static $ base ()), (0, 2, Static $ base ()), (2, 3, Static $ base ())])

addWildLin :: (RandomGen g, Graph gr) => Landscape -> (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
addWildLin w fp = do
	size <- landscapeSize w
	let loc = location w size
	p <- fp loc
	return $ Exp
		(mkGraph
			[(0, \(r, es) -> length es <= 4 && not (landscape (label r) `elem` [CITY, Village, Garrison, Cathedral]))]
			[])
		(mkGraph
			[(0, Copy 0), (1, Static $ fillBase p loc)]
			[(0, 1, Static $ base ())])

-- don't put deserts right next to water (or forests for that matter)
addAridLin :: (RandomGen g, Graph gr) => Landscape -> (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
addAridLin w fp = do
	size <- landscapeSize w
	let loc = location w size
	p <- fp loc
	return $ Exp
		(mkGraph
			[(0, \(r, es) -> length es <= 4 && not (landscape (label r) `elem` [CITY, Village, Cathedral, Lake, River, Swamp, Coast, Forest]))]
			[])
		(mkGraph
			[(0, Copy 0), (1, Static $ fillBase p loc)]
			[(0, 1, Static $ base ())])

addLinkingFrom :: (Foldable t, RandomGen g, Graph gr) => t Landscape -> Landscape -> (Location -> Rand g p) -> Rand g (Expansion gr (GenNode p Location) (GenEdge ()))
addLinkingFrom from add fp = do
	size <- landscapeSize add
	let loc = location add size
	p <- fp loc
	return $ Exp
		(mkGraph
			[(0, \(r, es) -> length es <= 4 && landscape (label r) `elem` from)]
			[])
		(mkGraph
			[(0, Copy 0), (1, Static $ fillBase p loc)]
			[(0, 1, Static $ base ())])

base :: a -> GenEdge a
base a = ED Open 0 a

label :: GenNode p a -> a
label = _a

landscapeGeneratorMetric :: RandomGen g => (Int -> p)
	-> (Location -> Rand g p)
	-> Embedding g p Gr (GenNode p Location) (GenEdge ())
	-> Int
	-> Rand g (GraphGrammar g p () Gr (GenNode p Location) (GenEdge ()))
landscapeGeneratorMetric starter fp embedding n = do
	landscape <- baseLandscapeMetric starter
	return $ GG
		landscape
		()
		(forNullAcc <$>
			[ roadLink Village fp -- only appear b/t two roads
			, roadLink Garrison fp
			, expandHighroad fp -- spread out space b/t city/village/garrison and other roads
			, roadBranch Village fp -- branch off with a highroad leading to it
			, roadBranch Garrison fp
			, addLinkingFrom [CITY,Village,Garrison] Church fp
	--		, roadBranch Ruins
			, branchUrban Fields fp
			, branchUrban Junkheap fp -- branch off any urban area
	--		, branchUrban Ruins-- (urban: city/village/garrison/highroad)
			, addWildTri Forest fp -- branch from any two non-urban locations
			, addWildTri Lake fp
			, addWildTri Hills fp
			, addWildCoast fp -- branch from one river/lake and one non-urban, adding a canyon and coast
			, addWildLin Prarie fp -- make a linear connection from any non-populated location
			, addWildLin Wastes fp -- (populated: urban minus highroad)
			, addAridLin Desert fp
			, addLinkingFrom [Canyon, Lake] River fp
			, addLinkingFrom [Canyon, Lake, River] Coast fp
			, addLinkingFrom [Mountain] Caverns fp
			, addLinkingFrom [Mountain] Swamp fp
			, addLinkingFrom [Mountain] Hills fp
			, addLinkingFrom [Mountain] Mountain fp
			])
		embedding
		(\x -> stopAtSize n x) -- && length (nub $ label . snd <$> labNodes x) >= 19)
		(Just $ flipGenEdge flipId)
		embedGenNode

-- TODO: add in expansion weighing, based on size maybe, so that it can mostly-expand road networks at the beginning, and then switch over to mostly expanding wilderness after that.
-- requires at least one of all landscape types and also at least n nodes
{-
landscapeGenerator :: RandomGen g => Int -> Rand g (GraphGrammar g (V3 Float) () Gr (GenNode (V3 Float) Location) (GenEdge ()))
landscapeGenerator n = do
	landscape <- baseLandscape
	return $ GG
		landscape
		()
		(forNullAcc <$>
			[ roadLink Village (\_ -> pure fallback) -- only appear b/t two roads
			, roadLink Garrison (\_ -> pure fallback)
			, expandHighroad (\_ -> pure fallback) -- spread out space b/t city/village/garrison and other roads
			, roadBranch Village (\_ -> pure fallback) -- branch off with a highroad leading to it
			, roadBranch Garrison (\_ -> pure fallback)
			, addLinkingFrom [CITY,Village,Garrison] Church (\_ -> pure fallback)
	--		, roadBranch Ruins
			, branchUrban Fields (\_ -> pure fallback)
			, branchUrban Junkheap (\_ -> pure fallback) -- branch off any urban area
	--		, branchUrban Ruins-- (urban: city/village/garrison/highroad)
			, addWildTri Forest (\_ -> pure fallback) -- branch from any two non-urban locations
			, addWildTri Lake (\_ -> pure fallback)
			, addWildTri Hills (\_ -> pure fallback)
			, addWildCoast (\_ -> pure fallback) -- branch from one river/lake and one non-urban, adding a canyon and coast
			, addWildLin Prarie (\_ -> pure fallback) -- make a linear connection from any non-populated location
			, addWildLin Wastes (\_ -> pure fallback) -- (populated: urban minus highroad)
			, addAridLin Desert (\_ -> pure fallback) 
			, addLinkingFrom [Canyon, Lake] River (\_ -> pure fallback)
			, addLinkingFrom [Canyon, Lake, River] Coast (\_ -> pure fallback)
			, addLinkingFrom [Mountain] Caverns (\_ -> pure fallback)
			, addLinkingFrom [Mountain] Swamp (\_ -> pure fallback)
			, addLinkingFrom [Mountain] Hills (\_ -> pure fallback)
			, addLinkingFrom [Mountain] Mountain (\_ -> pure fallback)
			])
		landscapeEmbed
		(\x -> stopAtSize n x) -- && length (nub $ label . snd <$> labNodes x) >= 19)
		(Just $ flipGenEdge flipId)
		embedGenNode
-}
forNullAcc :: Applicative m => m v -> (() -> m (v, ()))
forNullAcc v = \_ -> (,) <$> v <*> pure ()

-- uh obviously this would be like, step one. this sets a reasonable "starting" position for a new node (between its connected nodes / on its creator node), and then the next step would be spring/relaxation steps to push everything apart
landscapeEmbed :: (RandomGen g, Graph gr) => Embedding g (V3 Float) gr (GenNode (V3 Float) Location) (GenEdge ())
landscapeEmbed gr new = Just $ pure $ \i -> if i `elem` new
	then case context gr i of
		(in_, _, _, out) -> average $ getPos . snd <$> in_ ++ out
	else getPos i
	where
		getPos :: Int -> V3 Float
		getPos j = case lab gr j of
			Nothing -> error "missing node"
			Just (ND x _ _ _) -> x

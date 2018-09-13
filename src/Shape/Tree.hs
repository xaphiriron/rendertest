module Shape.Tree
	( pineTrees
	, broadleafTrees
	, palmTrees
	, cedarTrees
	, TreeMaterial(..)
	, treeMatColor

	, rocks
	, rockClusters
	, RockMaterial(..)
	, rockMatColor

	, grasses
	) where

import Prelude hiding (flip)

import Control.Applicative
import Data.List

import Linear.V2
import Linear.V3
import Linear.Vector
import Data.PossibilitySpace
import Data.Color
import Data.Turtle
import Data.Turtle.Polyhedra

import Shape.Hex hiding (Shaped(..))
import Shape

data PineTree = Pine Int Int TreeMaterial
	deriving (Eq, Ord, Show, Read)

data TreeMaterial
	= DarkWood | LightWood
	| Greenish | Bluish | Yellowish
	| DarkGreen
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

pineTrees :: RenderSpace PineTree TreeMaterial ColorRGB
pineTrees = RenderSpace pineTree renderPine (const treeMatColor)

treeMatColor :: TreeMaterial -> ColorRGB
treeMatColor m = case m of
	DarkWood  -> RGB8 0x90 0x60 0x20
	LightWood -> RGB8 0xc0 0xa0 0x40
	Greenish  -> RGB8 0xa0 0xe0 0x70
	Bluish    -> RGB8 0x70 0xe0 0xc0
	Yellowish -> RGB8 0xd0 0xe0 0x80
	DarkGreen -> RGB8 0x40 0x60 0x40

pineTree :: PossibilitySpace PineTree
pineTree = Pine
	<$> rangeNum (1,4)
	<*> rangeNum (0, 59)
	<*> from [Greenish, Bluish]

renderPine :: PineTree -> [TRecord TreeMaterial]
renderPine (Pine tuftCount tuftRotation color) = mconcat
	[ trunk
	, fmap (rotateAroundY $ fromIntegral tuftRotation) $ concat $ take tuftCount $ (\d -> tuft 15 (d * 10) (d `mod` 2 == 0) color) <$> [1..]
	]

tuft :: Int -> Int -> Bool -> TreeMaterial -> [TRecord TreeMaterial]
tuft size height rot material = fmap
			(translateRecord (V3 0 h 0)
			. rotateAroundY t
			. (fmap $ const material))
		$ cone 3 (fromIntegral size) (fromIntegral size)
	where
		t = if rot
			then 60 / 180 * pi
			else 0
		h = fromIntegral height * (-1)

trunk :: [TRecord TreeMaterial]
trunk = fmap (fmap $ const DarkWood) $ cone 6 5 25

data BroadleafTree = Broadleaf Int Int Int Int TreeMaterial
	deriving (Eq, Ord, Show, Read)

broadleafTrees :: RenderSpace BroadleafTree TreeMaterial ColorRGB
broadleafTrees = RenderSpace broadleafTree renderBroadleaf (const treeMatColor)

broadleafTree :: PossibilitySpace BroadleafTree
broadleafTree = Broadleaf
	<$> rangeNum (15, 30)
	<*> rangeNum (20, 35)
	<*> rangeNum (0, 89)
	<*> rangeNum (0, 89)
	<*> from [Greenish, Yellowish]

renderBroadleaf :: BroadleafTree -> [TRecord TreeMaterial]
renderBroadleaf (Broadleaf trunkHeight canopyHeight trunkRot canopyRot leafMat) = mconcat
	[ rotateAroundY (fromIntegral trunkRot) . fmap (const LightWood) <$> prism 4 (fromIntegral <$> V3 2 trunkHeight 2)
	, rotateAroundY (fromIntegral canopyRot) . translateRecord (V3 0 h 0) . fmap (const leafMat) <$> capsule 4 0.5 (V3 6 canopyHeight 6)
	]
	where
		h = fromIntegral trunkHeight * (-1)

data RockCluster = RC [Rock]

rockCluster :: RockMaterial -> PossibilitySpace RockCluster
rockCluster mat = RC <$> (drawsOfNRange (2, 3) $ rock mat)

renderRockCluster :: RockCluster -> [TRecord RockMaterial]
renderRockCluster (RC rs) = renderRock =<< rs

rockClusters :: RockMaterial -> RenderSpace RockCluster RockMaterial ColorRGB
rockClusters mat = RenderSpace (rockCluster mat) renderRockCluster (const rockMatColor)

data Rock = Rock RockMaterial (V2 Int) Int Int Int Int
	deriving (Eq, Ord, Show, Read)

data RockMaterial = Sandstone | Granite | Basalt
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

rock :: RockMaterial -> PossibilitySpace Rock
rock mat = Rock
	<$> pure mat
	<*> (V2 <$> rangeNum (-6, 6) <*> rangeNum (-6, 6))
	<*> rangeNum (4, 7)
	<*> rangeNum (4, 8)
	<*> rangeNum (2, 8)
	<*> rangeNum (1, 2)

renderRock :: Rock -> [TRecord RockMaterial]
renderRock (Rock mat (V2 ox oz) sides width height slope) =
	translateRecord (fromIntegral <$> V3 ox 0 oz) . fmap (const mat)
		<$> taperedPrism sides (fromIntegral width) (fromIntegral $ max 0 $ width - (height * slope `div` 2)) (fromIntegral $ height * 2)

rocks :: RockMaterial -> RenderSpace Rock RockMaterial ColorRGB
rocks mat = RenderSpace (rock mat) renderRock (const rockMatColor)

rockMatColor :: RockMaterial -> ColorRGB
rockMatColor m = case m of
	Sandstone -> RGB8 0xc0 0xc0 0x40
	Granite -> RGB8 0x90 0x90 0xa0
	Basalt -> RGB8 0x48 0x40 0x48

data Grass
	= GrassHex Stalk Stalk Stalk Stalk Stalk Stalk Stalk
	| GrassTri Stalk Stalk Stalk

data Stalk = Stalk Int Int
	deriving (Eq, Ord, Show, Read)

stalk = Stalk <$> rangeNum (2, 9) <*> rangeNum (0, 89)

renderStalk :: Stalk -> [TRecord TreeMaterial]
renderStalk (Stalk height rotation) = rotateAroundY t . fmap (const Greenish) <$> prism 4 (fromIntegral <$> V3 1 height 1)
	where
		t = fromIntegral rotation / 180 * pi

grasses :: RenderSpace Grass TreeMaterial ColorRGB
grasses = RenderSpace grass renderGrass (const treeMatColor)

grass :: PossibilitySpace Grass
grass = (GrassTri <$> stalk <*> stalk <*> stalk)
	<|> (GrassHex <$> stalk <*> stalk <*> stalk <*> stalk <*> stalk <*> stalk <*> stalk)

renderGrass :: Grass -> [TRecord TreeMaterial]
renderGrass g = case g of
	GrassTri a b c -> mconcat
		[ translateRecord (triPoint 0) <$> renderStalk a
		, translateRecord (triPoint 1) <$> renderStalk b
		, translateRecord (triPoint 2) <$> renderStalk c
		]
	GrassHex a b c d e f g -> mconcat
		[ translateRecord (hexPoint 0) <$> renderStalk a
		, translateRecord (hexPoint 1) <$> renderStalk b
		, translateRecord (hexPoint 2) <$> renderStalk c
		, translateRecord (hexPoint 3) <$> renderStalk d
		, translateRecord (hexPoint 4) <$> renderStalk e
		, translateRecord (hexPoint 5) <$> renderStalk f
		, translateRecord (hexPoint 6) <$> renderStalk g
		]
	where
		triPoint n = case n of
			0 -> 0.25 * (asV3 $ adjacent (Hex 0 0) !! 5)
			1 -> 0.25 * (asV3 $ adjacent (Hex 0 0) !! 4)
			2 -> 0.25 * (asV3 $ Hex (-2) (-1))
			_ -> 0
		hexPoint n = case n of
			x | x < 6 -> 0.33 * (asV3 $ adjacent (Hex 0 0) !! x)
			_ -> 0
		asV3 :: Hex -> V3 Float
		asV3 = (\(x, z) -> V3 x 0 z) . render

data CedarTree = CedarTree Int Int Int Float TreeMaterial
	deriving (Eq, Ord, Show, Read)

cedarTree :: PossibilitySpace CedarTree
cedarTree = CedarTree
	<$> rangeNum (20, 30)
	<*> rangeNum (3, 6)
	<*> rangeNum (40, 50)
	<*> ((\d -> pi * fromIntegral d / 180) <$> rangeNum (0, 59))
	<*> from [Greenish, DarkGreen]

cedarTrees :: RenderSpace CedarTree TreeMaterial ColorRGB
cedarTrees = RenderSpace cedarTree renderCedar (const treeMatColor)

renderCedar :: CedarTree -> [TRecord TreeMaterial]
renderCedar (CedarTree trunkHeight baseHeight topHeight rot leafColor) = rotateAroundY rot <$>
	mconcat
		[ fmap (const LightWood) <$> prism 4 (fromIntegral <$> V3 3 trunkHeight 3)
		, translateRecord (V3 0 (fromIntegral (trunkHeight + baseHeight) * (-1)) 0)
			. fmap (const leafColor)
				<$> flip (base <> sideCap)
		, translateRecord (V3 0 (fromIntegral (trunkHeight + baseHeight) * (-1)) 0)
			. fmap (const leafColor)
				<$> (tip <> sideTop)
		]
	where
		(sideCap, base, _) = splitTaperedPrism 3 15 10 (fromIntegral baseHeight)
		(sideTop, tip, _) = splitTaperedPrism 3 15 3 (fromIntegral topHeight)

data PalmTree = PalmTree Int Int Int Float
	deriving (Eq, Ord, Show, Read)

palmTree :: PossibilitySpace PalmTree
palmTree = PalmTree
	<$> rangeNum (21, 35)
	<*> rangeNum (5, 10)
	<*> rangeNum (4, 7)
	<*> ((\d -> pi * fromIntegral d / 180) <$> rangeNum (0, 59))

palmTrees :: RenderSpace PalmTree TreeMaterial ColorRGB
palmTrees = RenderSpace palmTree renderPalm (\_ -> treeMatColor)

renderPalm :: PalmTree -> [TRecord TreeMaterial]
renderPalm (PalmTree trunkHeight leafSize leafCount rotation) = fmap (rotateAroundY rotation) $ mconcat
	[ fmap (const LightWood) <$> cone 6 3 (fromIntegral trunkHeight)
	, leaves
	]
	where
		leaves = (\t -> rotateAroundY t <$> leaf) =<< take leafCount (iterate (+ leafTheta) 0)
		leaf = translateRecord (V3 (fromIntegral leafSize * (-1)) (fromIntegral trunkHeight * (-1)) 0) . fmap (const Greenish)
			<$> octohedron (V3 (fromIntegral leafSize) 2 4)
		leafTheta = pi / (fromIntegral leafCount / 2)

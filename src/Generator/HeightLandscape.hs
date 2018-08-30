module Generator.HeightLandscape
	( landscapeGraph
	, landscapeColor
	, Landscape(..)
	, Location(..)
	, Connection(..)

	) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe

import Data.Graph.Inductive

import Utility.Rand
import Utility.Shuffle

import Data.Color
import Data.GraphGrammar
import Data.PossibilitySpace
import Shape.Hex
import Shape.Hex.ShapeEmbedding

import Debug.Trace

-- features aren't used at all yet
data UplandFeature
	= Village
	| City
	| Castle
	| Temple
	| Monolith
	| Tower
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

data UndergroundFeature
	= Cave
	| Dungeon
	| Chasm
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Feature = Up UplandFeature | Under UndergroundFeature
	deriving (Eq, Ord, Show, Read)

data Landscape
	= Forest
	| Plain
	| Desert | Badland | Wasteland | Mesa
	| Hills | Mountain
	| Coast | Canyon
	| Swamp | Marsh | Lake
	| Tundra | Glacier
	| Ocean
	| LakeBed | SwampBottom | OceanFloor
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

landscapeColor :: Landscape -> ColorRGB
landscapeColor l = case l of
	Forest -> RGB8 0xa0 0xf0 0x30
	Plain -> RGB8 0xb0 0xb0 0x40
	Desert -> RGB8 0xd0 0xd0 0x30
	Badland -> RGB8 0xa0 0x90 0x50
	Wasteland -> RGB8 0xa0 0xa0 0x60
	Mesa -> RGB8 0xf0 0x90 0x10
	Hills -> RGB8 0xa0 0xa0 0xa0
	Mountain -> RGB8 0xf0 0xf0 0xf0
	Coast -> RGB8 0xc0 0xb0 0x40
	Canyon -> RGB8 0xf0 0xc0 0x90
	Swamp -> RGB8 0x50 0xc0 0x80
	Marsh -> RGB8 0x40 0xb0 0xc0
	Lake -> RGB8 0x20 0x60 0xf0
	Tundra -> RGB8 0xa0 0xa0 0x0d0
	Glacier -> RGB8 0xc0 0xc0 0xd0
	Ocean -> RGB8 0x10 0x10 0xf0

	SwampBottom -> RGB8 0xf0 0xe0 0xa0
	LakeBed -> RGB8 0xf0 0xe0 0xa0
	OceanFloor -> RGB8 0x70 0x70 0x20

data Location = Location Landscape Int Shape [Feature]
	deriving (Eq, Ord, Show, Read)

data Road = Wild | Road
	deriving (Eq, Ord, Enum, Bounded, Show, Read)
data Water = Dry | River Flow Int | Aqueduct Flow Int
	deriving (Eq, Ord, Show, Read)
data Flow = Inflow | Outflow
	deriving (Eq, Ord, Enum, Bounded, Show, Read)
data Connection = Connection Road Water
	deriving (Eq, Ord, Show, Read)

landscapeGraph :: RandomGen g => Landscape -> [Landscape] -> Int -> Rand g (Either GenError (Gr Location Connection))
landscapeGraph starting allowed sizeLimit = do
	start <- roll $ Location starting
		<$> landscapeHeight starting
		<*> landscapeShape starting
		<*> pure []
	generate $ GG (mkGraph [(0, start)] [])
		() -- expansion context
		((makeLink <$> allowed)
		<> (makeLinkTri <$> allowed)
			) -- expansion deck
		(shapeEmbedding (const True)
			(\(Location _ _ s _) -> s)
			embedLocation)
		(totalSize sizeLimit)
		(Just $ Flip $ \(Connection r w) -> Connection r $ flipWater w)
		embedLocation
	where
		embedLocation = Embedder $ \s (Location l h _ f) -> Location l h s f
		totalSize max gr = sum (shapeSize . (\(Location _ _ s _) -> s) . snd <$> labNodes gr) >= max

addFeature :: (RandomGen g, Graph gr) => [Feature] -> [Landscape] -> a -> Rand g (Expansion gr Location Connection, a)
addFeature features lands acc = do
	ft <- fromMaybe (error "no features") <$> randomFromList features
	return $
		( Exp
			(mkGraph
				[ (0, \(Location land _ shape fts, _) ->
					land `elem` lands && shapeSize shape > length fts
					)
				]
				[])
			(mkGraph
				[ (0, Mutate 0 $ \(Location land height shape fts) ->
					Location land height shape (ft : fts)
					)
				]
				[])
		, acc
		)


makeLink :: (Graph gr, RandomGen g) => Landscape -> a -> Rand g (Expansion gr Location Connection, a)
makeLink to acc = do
	let newLand = to
	shape <- roll $ landscapeShape newLand
	heights <- liftRand $ shuffle $ enumerate $ landscapeHeight newLand

	return $
		( Exp
			(mkGraph
				[ (0, \(Location land h _ _, _) ->
						{- let debug = unlines
							["available lands: " <> show (landscapeAttachments land)
							,"drew " <> show newLand <> "; member test: " <> show (newLand `Set.member` landscapeAttachments land)
							, "old height:" <> show h <> "/" <> "possible heights:" <> show heights
							,"available heights:" <> show (landscapeAvailableHeight newLand [h])
							]
						in trace debug $ -} (newLand `Set.member` landscapeAttachments land)
							&& isJust (landscapeAvailableHeight newLand [(land, h)]) )
				]
				[])
			(mkGraph
				[ (0, Copy 0)
				, (1, Mutate 0 $ \(Location land h _ _) ->
					Location newLand
						(firstIn (fromMaybe (error "failed height check after passing it previously") $ landscapeAvailableHeight newLand [(land, h)]) heights)
						shape []
					)
				]
				[ (0, 1, Static $ Connection Wild Dry)
				])
		, acc
		)

makeLinkTri :: (Graph gr, RandomGen g) => Landscape -> a -> Rand g (Expansion gr Location Connection, a)
makeLinkTri to acc = do
		shape <- roll $ landscapeShape to
		heights <- liftRand $ shuffle $ enumerate $ landscapeHeight to
		return $
			( ExpContext
				(mkGraph
					[ (0, \(Location land h _ _, _) -> fitsAdjacentTo to land h)
					, (1, \(Location land h _ _, _) -> fitsAdjacentTo to land h)
					]
					[ (0, 1, \_ -> True)
					])
				(\matchedGr -> case (labNodes matchedGr, labEdges matchedGr) of
					(
						[ (0, Location land h sh ft)
						, (1, Location land' h' sh' ft')
						]
						,
						[ (0, 1, e)
						]
						) -> case landscapeAvailableHeight to [(land, h), (land', h)] of
						Nothing -> Nothing
						Just hs -> let
								h = firstIn hs heights
							in Just $
									mkGraph
										[ (0, Location land h sh ft)
										, (1, Location land' h' sh' ft')
										, (2, Location to h shape [])
										]
										[ (0, 1, e)
										, (0, 2, Connection Wild Dry)
										, (1, 2, Connection Wild Dry)
										]
					_ -> Nothing
				)
			, acc
			)
	where
		fitsAdjacentTo newLand land h =
			(newLand `Set.member` landscapeAttachments land)
				&& isJust (landscapeAvailableHeight newLand [(land, h)])
{-
shiftLink ::
splitLink ::
-}

firstIn :: Ord a => Set a -> [a] -> a
firstIn set = go
	where
		go [] = error "empty firstIn"
		go (h:rs) = if h `Set.member` set
			then h
			else go rs

landscapeAvailableHeight :: Landscape -> [(Landscape, Int)] -> Maybe (Set Int)
landscapeAvailableHeight check adjs = case Set.intersection allValidHeights allowedAdjacentHeights of
		x | Set.null x -> Nothing
			| otherwise -> Just x
	where
		allValidHeights = Set.fromList . enumerate $ landscapeHeight check
		adjHeights land i =
			let
				r = landscapeFaultRange land
				relRange = [0..r] <> ((* (-1)) <$> [1..r])
			in Set.fromList $ filter (>= 0) $ (+ i) <$> relRange
		allowedAdjacentHeights = intersections $ uncurry adjHeights <$> adjs
		intersections :: Ord a => [Set a] -> Set a
		intersections [] = mempty
		intersections (h:rs) = foldr Set.intersection h rs

landscapeHeight :: Landscape -> PossibilitySpace Int
landscapeHeight l = case l of
	Forest -> rangeNum (10, 32)
	Plain -> rangeNum (8, 24)
	Desert -> rangeNum (8, 20)
	Badland -> rangeNum (12, 20)
	Wasteland -> rangeNum (12, 20)
	Mesa -> rangeNum (16, 28)
	Hills -> rangeNum (24, 26)
	Mountain -> rangeNum (32, 48)
	Coast -> rangeNum (0, 8)
	Canyon -> rangeNum (4, 24)
	Swamp -> rangeNum (4, 12)
	Marsh -> rangeNum (4, 12)
	Lake -> rangeNum (4, 48)
	Tundra -> rangeNum (4, 36)
	Glacier -> rangeNum (0, 48)
	Ocean -> pure 0

	-- these are less 'biomes' and more 'terrain tiles'
	LakeBed -> pure (-1)
	SwampBottom -> pure (-1)
	OceanFloor -> pure (-1)

landscapeFaultRange :: Landscape -> Int
landscapeFaultRange l = case l of
	Forest -> 8
	Plain -> 4
	Desert -> 4
	Badland -> 8
	Wasteland -> 8
	Mesa -> 4
	Hills -> 8
	Mountain -> 16
	Coast -> 16
	Canyon -> 16
	Swamp -> 8
	Marsh -> 8
	Lake -> 4
	Tundra -> 8
	Glacier -> 16
	Ocean -> 32

	LakeBed -> 0
	SwampBottom -> 0
	OceanFloor -> 0

landscapeShape :: Landscape -> PossibilitySpace Shape
landscapeShape l = case l of
	Forest -> hexSize 1 4
	Plain -> hexSize 1 4
	Desert -> hexSize 2 4
	Badland -> hexSize 1 6
	Wasteland -> hexSize 1 6
	Mesa -> hexSize 2 6
	Hills -> hexSize 2 4
	Mountain -> hexSize 1 3
	Coast -> hexSize 2 4
	Canyon -> hexSize 1 4
	Swamp -> hexSize 1 4
	Marsh -> hexSize 1 4
	Lake -> hexSize 2 5
	Tundra -> hexSize 3 7
	Glacier -> hexSize 4 10
	Ocean -> hexSize 5 12

	LakeBed -> hexSize 0 0
	SwampBottom -> hexSize 0 0
	OceanFloor -> hexSize 0 0
	where
		hexSize lo hi = HSize 0 <$> rangeNum (lo, hi)

landscapeAttachments :: Landscape -> Set Landscape
landscapeAttachments l = Set.fromList $ case l of
	Forest -> [Forest, Plain, Hills, Coast, Canyon, Swamp, Marsh, Lake]
	Plain -> [Plain, Forest, Desert, Hills, Coast, Canyon, Swamp, Marsh, Lake]
	Desert -> [Desert, Plain, Badland, Wasteland, Mesa, Hills, Coast, Canyon]
	Badland -> [Badland, Desert, Wasteland, Mesa, Hills, Coast, Canyon]
	Wasteland -> [Wasteland, Desert, Badland, Mesa, Hills, Coast, Canyon]
	Mesa -> [Mesa, Desert, Badland, Wasteland, Mesa, Hills, Coast, Canyon]
	Hills -> [Hills, Mountain, Plain, Forest]
	Mountain -> [Mountain, Hills]
	Coast -> [Coast, Forest, Plain, Desert, Canyon, Swamp, Marsh, Ocean]
	Canyon -> [Canyon, Coast, Lake]
	Swamp -> [Coast, Plain, Ocean, Hills]
	Marsh -> [Coast, Plain, Ocean, Lake, Hills]
	Lake -> [Forest, Plain, Marsh, Hills]
	Tundra -> [Ocean, Tundra, Glacier]
	Glacier -> [Tundra, Glacier]
	Ocean -> [Ocean, Coast, Tundra]

	LakeBed -> []
	SwampBottom -> []
	OceanFloor -> []

flipWater :: Water -> Water
flipWater Dry = Dry
flipWater (River flow a) = River (flipFlow flow) a
flipWater (Aqueduct flow a) = Aqueduct (flipFlow flow) a

flipFlow :: Flow -> Flow
flipFlow Inflow = Outflow
flipFlow Outflow = Inflow

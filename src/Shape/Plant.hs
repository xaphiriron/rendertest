module Shape.Plant
	( size

	, GeneticPlant(..)
	, geneticPlants
	, plantIndex
	, geneticPlantsRender

	, branchingPlants
	, bplantIndex

	) where

import Control.Monad.Identity
import Control.Applicative
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid

import Data.Fixed
import Linear.V2
import Linear.Metric

import Data.Color

import Data.PossibilitySpace
import Data.LSystem
import Data.Turtle
import Utility.SVGRender
import qualified Shape.PlantTypes as T (Plant(..))

import Shape

plantIndex :: GeneticPlant -> T.Plant GIndex Identity (TurtleSymbol GIndex TPlant)
plantIndex gen = T.PlantIndexed
	(plantPostProcess gen)
	(lSystemFromPlant gen)
	[A $ Apex 0 (apexGrowthSteps gen)]
	(GStem 0 (apexGrowthSteps gen))
	(gindexColor gen)

gindexColor :: GeneticPlant -> GIndex -> SVGDrawData
gindexColor gen =
	(\g -> case g of
		GStem w i -> let
				toHSL f = HSL f f f
				realWidth :: Float
				realWidth = 0.05 * (1 + fromIntegral w / fromIntegral (stemWidthFade gen))
				realIndex :: ColorHSL Float
				realIndex = toHSL $ fromIntegral i / fromIntegral (stemColorFade gen)
			in
				strokeWidth realWidth <>
				(stroke $ hsl2rgb $ bounce
					$ (stemBaseColor gen) + (stemAdjustColor gen * realIndex))
		GLeaf i -> fill $ hsl2rgb $ bounce $ (leafBaseColor $ leafParams gen) + (leafAdjustColor (leafParams gen) * fromInteger (fromIntegral i))
		GBud -> fill $ hsl2rgb $ flowerColor gen
		GPedicel -> (stroke $ hsl2rgb $ pedicelColor gen) <> strokeWidth 0.05)
	where
		bounce (HSL h s l) = HSL h (b s) (b l)
			where
				b x = if m2 /= m1 then 1 - m1 else m1
					where
						m2 = x `mod'` 2
						m1 = x `mod'` 1


bplantIndex :: BranchingPlant -> T.Plant GIndex Identity (TurtleSymbol GIndex TPlant)
bplantIndex plant = T.PlantIndexed
	(bplantPostProcess plant)
	(lSystemFromBPlant plant)
	[A $ Apex 0 0]
	(GStem 0 0)
	(\g -> case g of
		GStem w i -> strokeWidth 0.1 <> stroke (RGB8 0x2 0x6 0x0)
		GLeaf i -> fill (RGB8 0x5 0xc 0x2)
		GBud -> fill (RGB8 0xf 0x2 0x0)
		GPedicel -> stroke (RGB8 0xf 0xf 0xf) <> strokeWidth 0.05)

{-
sunflower
palm tree
pine tree
lilac
cherry tree

flowering bush

-}

{-
herbacious plant params:
* how many lengths of offshoot branches does it generate?
	0: no branches, leaves directly on initial apex;
	1: branches off apex which themselves grow leaves
	... etc
* leaf winding angle (by what angle do internodes rotate)
* branching angle (by what angle do leaves or branches split from their base branch)
* generate single leaves or opposing pairs of leaves
* does apex growth terminate with bud?
	yes: terminal, sympodial; no: monopodial, polypodial
* does lateral growth terminate with bud?
	yes: terminal, monopodial; no: sympodial, polypodial
* do any of these values change over time
* how long are the internodes
-}
data GeneticPlant = GP
	{ branchDepth :: Int
	, forkCount :: Int
	, leafWinding :: Int
	, leafParams :: LeafP
	, branchingAngle :: Int
	, turnBeforeBranch :: Bool
	, branchBeforeInternode :: Bool
	, internodeAfterBranch :: Bool
	, internodeLength :: Int
	, generateOpposingLeaves :: Bool
	, stemBaseColor :: ColorHSL Float
	, stemAdjustColor :: ColorHSL Float
	, stemWidthFade :: Int
	, stemColorFade :: Int
	, terminateApexAtBud :: Bool
	, terminateLateralAtBud :: Bool
	, apexGrowthSteps :: Int
	, lateralGrowthSteps :: Int
	, lateralGrowthPenalty :: Int
	, pedicelLength :: Int
	, petalCount :: Int
	, petalVariety :: Petal
	, flowerColor :: ColorHSL Float
	, pedicelColor :: ColorHSL Float
	}
	deriving (Eq, Ord, Show, Read)

data LeafP = LeafP
	{ maxLeafSize :: Int
	, structure :: LeafStructure
	, leafBaseColor :: ColorHSL Float
	, leafAdjustColor :: ColorHSL Float
	}
	deriving (Eq, Ord, Show, Read)

data LeafStructure = LeafHex Float Int | LeafDiamond Float Int Int
	deriving (Eq, Ord, Show, Read)

data Petal = Diamond Int | Triangle Int | Square Int Int
	deriving (Eq, Ord, Show, Read)

geneticPlantsRender :: Int -> RenderSpace GeneticPlant GIndex SVGDrawData
geneticPlantsRender iterations = RenderSpace
	geneticPlants
	(\genome -> let T.PlantIndexed postProcess lsys seed z ix = plantIndex genome
		in runActions (defaultTurtle z) $ getActions
			$ postProcess =<< (evolve lsys iterations $ seed)
	)
	gindexColor



geneticPlants :: PossibilitySpace GeneticPlant
geneticPlants = GP
	<$> rangeNum (0,3)
	<*> rangeNum (1,3)
	<*> rangeNum (60, 300)
	<*> (LeafP
		<$> pure 14 -- rangeNum (1, 7)
		<*> ((LeafHex
			<$> (fromIntegral <$> rangeNum (5, 85))
			<*> rangeNum (5, 10))
			<|> (LeafDiamond
				<$> (fromIntegral <$> rangeNum (2, 10)) -- midpoint thickness
				<*> rangeNum (1, 3) -- base ratio component
				<*> rangeNum (1, 3) -- tip ratio component
				)
			)
		<*> (HSL
			<$> (fromIntegral <$> rangeNum (60, 180))
			<*> (((/ 100) . fromIntegral) <$> rangeNum (30, 70))
			<*> (((/ 100) . fromIntegral) <$> rangeNum (30, 70)))
		<*> (HSL
			<$> (((/ 10) . fromIntegral) <$> rangeNum (-50, 50))
			<*> (((/ 1000) . fromIntegral) <$> rangeNum (-50, 50))
			<*> (((/ 1000) . fromIntegral) <$> rangeNum (-50, 50))))
	<*> (rangeNum (-110, -30) <|> rangeNum (30, 110))
	<*> from [False, True]
	<*> from [False, True]
	<*> from [False, True]
	<*> pure 1
	<*> from [False, True]
	<*> (HSL
		<$> (fromIntegral <$> rangeNum (0, 359))
		<*> (((/ 100) . fromIntegral) <$> rangeNum (20, 80))
		<*> (((/ 100) . fromIntegral) <$> rangeNum (20, 80)))
	<*> (HSL
		<$> (((/ 5) . fromIntegral) <$> rangeNum (-100, 100))
		<*> (((/ 1000) . fromIntegral) <$> rangeNum (-100, 100))
		<*> (((/ 1000) . fromIntegral) <$> rangeNum (-100, 100)))
	<*> rangeNum (1,4)
	<*> rangeNum (1,4)
	<*> from [False, True]
	<*> from [False, True]
	<*> rangeNum (5,20)
	<*> rangeNum (5,20)
	<*> rangeNum (0,5)
	<*> rangeNum (5,20) -- 0.5 - 2.0
	<*> rangeNum (3,11)
	<*> ((Diamond <$> rangeNum (5,60))
		<|> (Triangle <$> rangeNum (5,60))
		<|> (Square <$> rangeNum (5,20) <*> rangeNum (10, 30)))
	<*> (HSL
		<$> (fromIntegral <$> rangeNum (0, 359))
		<*> (fromIntegral <$> rangeNum (10, 100))
		<*> (fromIntegral <$> rangeNum (10, 100)))
	<*> (HSL
		<$> (fromIntegral <$> rangeNum (0, 359))
		<*> (fromIntegral <$> rangeNum (10, 100))
		<*> (fromIntegral <$> rangeNum (10, 100)))


data BranchingPlant = BP
	{ growthTwist :: Int
	, growthAngleDepth :: Int
	, branchCount :: Int
	, branchParams :: [BranchState]
	, gnarl :: Bool
	, gnarlRoll :: Int
	, gnarlPitch :: Int
	}
	deriving (Eq, Ord, Show, Read)

data BranchState = BS
	{ twistMultiplier :: Int
	, growthMultiplier :: Float
	, expandDelay :: Int
	, internodeLengthens :: Int
	}
	deriving (Eq, Ord, Show, Read)

branchingPlants :: PossibilitySpace BranchingPlant
branchingPlants = BP
	<$> rangeNum (30, 330) -- roll around major axis when growing
	<*> rangeNum (10, 80) -- pitch around major axis when growing
	<*> rangeNum (1, 3)
	<*> rollsOfN 4 (BS
		<$> rangeNum (1, 9) -- the roll multiplier for successive branches
		<*> fmap ((/ 10) . fromIntegral) (rangeNum (5, 20)) -- the branch length multiplier
		<*> rangeNum (2,9) -- how many evaluation steps to take before branching again
		<*> rangeNum (0,4)) -- how many times the internode will lengthen before reaching its final length
	<*> from [False, True] -- do internodes have a kink in them
	<*> rangeNum (-90, 90) -- kink roll from major axis
	<*> rangeNum (-30, 30) -- kink pitch

bplantPostProcess :: BranchingPlant -> TurtleSymbol GIndex TPlant -> [TurtleSymbol GIndex TPlant]
bplantPostProcess plant sym = case sym of
	A (Internode w _) -> [TA $ Advance w]
	x -> [x]

lSystemFromBPlant :: BranchingPlant -> LSystem Identity (TurtleSymbol GIndex TPlant)
lSystemFromBPlant plant = dcfLSystem advanceFunc
	where
		advanceFunc sym = fromMaybe [sym] $ firstOf parts sym
		parts :: [TurtleSymbol GIndex TPlant -> Maybe [TurtleSymbol GIndex TPlant]]
		parts = [apex, delay, internode]

		apex (A (Apex a b)) = let
				down = TA $ Pitch $ deg2rad $ fromIntegral $ growthAngleDepth plant
				twist branch = TA $ Roll $ deg2rad $ fromIntegral
					$ twistMultiplier branch * growthTwist plant
				branch branch = [TA Push, down]
					<> segment branch
					<> [A $ DelayTo (expandDelay branch) $ Apex a b, TA Pop]
				segment branch = if gnarl plant
					then
						[ A $ Internode (0.5 * growthMultiplier branch) (internodeLengthens branch)
						, TA $ Roll $ deg2rad $ fromIntegral $ gnarlRoll plant
						, TA $ Pitch $ deg2rad $ fromIntegral $ gnarlPitch plant
						, A $ Internode (0.5 * growthMultiplier branch) (internodeLengthens branch)
						]
					else [ A $ Internode (1 * growthMultiplier branch) (internodeLengthens branch) ]
			in
				Just $ concat $ take (branchCount plant) $ (\r -> twist r : branch r) <$> branchParams plant
		apex _ = Nothing
		delay (A (DelayTo i v))
			| i <= 0 = Just [A v]
			| otherwise = Just [A $ DelayTo (i-1) v]
		delay _ = Nothing
		internode (A (Internode l i))
			| i > 0 = Just [A $ Internode (l * 1.4) (i - 1)]
			| otherwise = Just $ [A $ Internode l i]
		internode _ = Nothing


data GIndex = GStem Int Int | GLeaf Int | GBud | GPedicel
	deriving (Eq, Ord, Show, Read)

data TPlant = Apex Int Int | Internode Float Int | Lateral Int Int
	| Bud Int | Leaf Int Int
	| DelayTo Int TPlant
	deriving (Eq, Ord, Show, Read)

leafRenderActions :: LeafP -> Int -> Int -> [TurtleSymbol GIndex TPlant]
leafRenderActions params l s = 
	[ TA $ Color (GLeaf l)
	, TA $ DrawMode Polygon
	] <> case structure params of
		LeafHex angle length -> let
				i :: Float
				i = fromIntegral s * 0.25
				ei = i * fromIntegral length * 0.1
				th = deg2rad $ angle
				l_sub = TA $ Turn (-th)
				l_add = TA $ Turn th
				turn = TA $ Turn pi
			in
				[ l_add, TA $ Advance i, l_sub, TA $ Advance ei, l_sub, TA $ Advance i
				, l_add, turn
				, l_add, TA $ Advance i, l_sub, TA $ Advance ei, l_sub, TA $ Advance i
				]
		LeafDiamond thickness base tip -> let
				baseRatio, tipRatio :: Float
				baseRatio = fromIntegral base / fromIntegral tip
				tipRatio = 1 -- i.e., tip / tip
				(V2 cx cy) = V2 (((thickness * fromIntegral s) / 2) * 0.1) (baseRatio * fromIntegral s * 0.25)
				baseAngle = atan2 cx cy
				baseLength = distance 0 $ V2 cx cy
				(V2 tx ty) = V2 (((thickness * fromIntegral s) / 2) * 0.1) (tipRatio * fromIntegral s * 0.25)
				tipAngle = atan2 tx ty
				tipLength = distance 0 $ V2 tx ty
			in
				{- starting at base point: figure out the angle so that in {base} steps we hit {thickness / 2}. or rather we have three points of a triangle:
					here, the starting point, 0,0
					straight along from here {base * s} units
					left {thickness / 2 (* s?)} units
					and we want to know
				-}
				[ TA $ Turn baseAngle, TA $ Advance baseLength
				, TA $ Turn (-(baseAngle + tipAngle)), TA $ Advance tipLength
				, TA $ Turn (pi + (tipAngle * 2))
				, TA $ Advance tipLength, TA $ Turn (-(tipAngle + baseAngle))
				, TA $ Advance baseLength
				]
	<> [ TA $ DrawMode Record ]

plantPostProcess :: GeneticPlant -> TurtleSymbol GIndex TPlant -> [TurtleSymbol GIndex TPlant]
plantPostProcess plant sym = case sym of
	A (Leaf l s) ->
		[ TA Push ]
		<> leafRenderActions (leafParams plant) l s
		<> [ TA Pop ]
	A (Bud _) ->
		[ TA Push
		-- , pitch_down3
		, roll1
		] <> concat (replicate ps $ pedicel <> petal <> [petalRoll]) <>
		[ TA Pop
		]
	x -> [x]
	where
		ps = petalCount plant
		petalRoll = TA $ Roll $ deg2rad $ 360 / fromIntegral ps
		pedicel =
			[ TA Push, TA $ Color GPedicel, pitch_up1
			, TA $ Advance (fromIntegral (pedicelLength plant) * 0.1)
			, TA Pop
			]
		petal = case petalVariety plant of
			Square thick range -> let
					δ = deg2rad $ fromIntegral thick
					s_sub = TA $ Turn (-δ)
					s_add = TA $ Turn δ
				in
					[ TA Push, pitch_down3
					, TA $ DrawMode Record
					, TA $ Advance 1 -- should be something related to thickness + number of sides
					, TA $ Color GBud, TA $ DrawMode Polygon
					, TA $ Turn (pi/2)
					, TA $ Advance $ fromIntegral range / 10
					, TA $ Turn (pi/2)
					, TA $ Advance 1
					, TA $ Turn (pi/2)
					, TA $ Advance $ fromIntegral range / 10
					, TA Pop
					]
			Diamond thickness -> let
					δ = deg2rad $ fromIntegral thickness
					s_sub = TA $ Turn (-δ)
					s_add = TA $ Turn δ
				in
					[ TA Push, TA $ Color GBud, TA $ DrawMode Polygon
					, pitch_down3
					, s_sub, TA $ Advance 1, s_add, TA $ Advance 1, turn
					, s_sub, TA $ Advance 1, s_add, TA $ Advance 1
					, TA $ DrawMode Record
					, TA Pop
					]
			Triangle thickness -> let
					δ = deg2rad $ fromIntegral thickness
					s_sub = TA $ Turn (-δ)
					s_add = TA $ Turn δ
				in
					[ TA Push, TA $ Color GBud, TA $ DrawMode Polygon
					, pitch_down3
					, s_sub, TA $ Advance 1, turn
					, s_sub, TA $ Advance 1, s_add, TA $ Advance 1
					, TA $ DrawMode Record
					, TA Pop
					]
		x = deg2rad 18
		roll1 = TA $ Roll x
		pitch_up1 = TA $ Pitch (-x)
		pitch_down3 = TA $ Pitch (x * 3)
		turn = TA $ Turn pi


lSystemFromPlant :: GeneticPlant -> LSystem Identity (TurtleSymbol GIndex TPlant)
lSystemFromPlant plant = dcfLSystem advanceFunc
	where
		advanceFunc sym = fromMaybe [sym] $ firstOf parts sym
		parts :: [TurtleSymbol GIndex TPlant -> Maybe [TurtleSymbol GIndex TPlant]]
		parts = [apex, internode, leaf, gstem]

		apex (A (Apex i g)) = Just $
			(if branchBeforeInternode plant
				then
					[ TA $ Color $ GStem 0 g
					]
					<> branches (i + 1) (g-1)
					<> [ A $ Internode 1 $ internodeLength plant
					, TA $ Roll $ deg2rad $ fromIntegral $ leafWinding plant
					]
				else
					[ TA $ Color $ GStem 0 g
					, A $ Internode 1 $ internodeLength plant
					, TA $ Roll $ deg2rad $ fromIntegral $ leafWinding plant
					] <>
					branches (i + 1) (g-1))
			<> if g > 0
				then [A $ Apex i (g-1)]
				else (if (i == 0 && terminateApexAtBud plant) ||
						(i /= 0 && terminateLateralAtBud plant)
					then [A $ Bud 0]
					else [A $ Apex i $ apexGrowthSteps plant])
		apex _ = Nothing
		branches depth g = let c = forkCount plant
			in concat $ take c $ iterate (\i -> (TA $ Roll $ deg2rad $ fromIntegral $ leafWinding plant) : i) $ branch depth g
		branch depth g =
			(if turnBeforeBranch plant
				then
					[ TA $ Roll $ deg2rad $ fromIntegral $ (* (-1)) $ branchingAngle plant
					, TA Push
					, TA $ Pitch $ deg2rad $ fromIntegral $ branchingAngle plant
					]
				else
					[ TA $ Push
					, TA $ Pitch $ deg2rad $ fromIntegral $ branchingAngle plant
					])
			<>
			[ TA $ Color $ GStem 0 g
			] <>
			(if internodeAfterBranch plant
				then [A $ Internode 1 $ internodeLength plant]
				else []) <>
			(if branchDepth plant <= depth
				then (if generateOpposingLeaves plant
					then
						[ A $ Leaf g 1
						, TA $ Roll $ pi -- since leaves don't do the branching tilt themselves, i think this just makes a double-sided polygon. hence all the z-fighting
						, A $ Leaf g 1
						]
					else
						[ A $ Leaf g 1
						])
				else (if g > 0
					then [ A $ Apex depth (g - lateralGrowthPenalty plant - 1) ]
					else []))
			<> [ TA $ Pop
			]
		internode (A (Internode l i))
			| i <= 0 = Just [A $ Internode l 0]
			| otherwise  = Just [TA $ Advance 1, A $ Internode l (i-1)]
		internode _ = Nothing
		leaf (A (Leaf l s))
			| s >= maxLeafSize (leafParams plant) = Just [A $ Leaf (l+1) s]
			| otherwise = Just [A $ Leaf (l+1) (s+1)]
		leaf _ = Nothing
		gstem (TA (Color (GStem w i))) = Just [TA $ Color $ GStem (w+1) i]
		gstem _ = Nothing
{-
(where X is a terminating growth)
The terminal and sympodial patterns are characterized by rules of the form
	A → I[B]n[X]mX,
with n = 0, m > 0 in the case of terminal patterns and n > 0, m ≥ 0 in the case of sympodial patterns.

Monopodial and polypodial patterns have rules of the form
	A → I[B]n[X]mC,
with n = 0, m > 0 in the case of monopodial patterns and n > 0, m ≥ 0 in the case of polypodial ones.
-}

{-
data LSystem a = LSystem
	{ advance :: a -> [a]
	}
-}

firstOf :: [a -> Maybe b] -> a -> Maybe b
firstOf funcs a = listToMaybe $ ($ a) `mapMaybe` funcs


data Size = S Bool Bool Bool Bool
	deriving (Eq, Ord, Show, Read)

size :: Size -> Int
size (S a b c d) = sum $ fmap snd $ filter fst $ zip [a,b,c,d] [1,2,3,4]

sized :: PossibilitySpace Size
sized = S <$> bounds <*> bounds <*> bounds <*> bounds
{-
plants :: PossibilitySpace Plant
plants = Plant <$> bounds <*> bounds <*> bounds
	<*> sized
	<*> pure 1 -- from [1..5]
	<*> pure 6 -- from [6..26]
	<*> from [1..20]
	<*> bounds
	<*> sized
	<*> sized
	<*> from [1..4]
	<*> sized
	<*> sized
-}

module Shape.PlantTypes
	( Plant(..)

	, Honda(..)
	, HondaParams(..)
	, defaultHonda
	, hondaSeed
	, hondaAdvance

	, Ternary(..)
	, TernaryParams(..)
	, defaultTernary
	, defaultTernarySeed
	, ternaryAdvance

	, Flowering(..)
	, floweringDefaults
	, floweringAdvance
	, floweringSeed
	, floweringColorIndex
	, floweringPlant

	, Bush(..)
	, bushAdvance
	, bushSeed
	, bushColorIndex
	, bush

	, HogewegHesper(..)
	, hogewegHesperLSystem
	, hogewegHesperSeed
	, hogewegHesperAdvance

	, hogewegHesperA
	, hogewegHesperB
	, hogewegHesperC
	, hogewegHesperD
	, hogewegHesperE

	, MycelisMuralisIII(..)
	, mycelisMuralisIIILSystem
	, mycelisMuralisIIISeed
	, mycelisMuralisIIIAdvance
	, mycelisMuralisIIIPostProcess
	, mycelisMuralisIIIColorIndex
	, mycelisMuralisIII
	) where
-- H. Honda. Description of the form of trees by the parameters of
-- the tree-like body: Effects of the branching angle and the branch
-- length on the shape of the tree-like body. Journal of Theoretical
-- Biology, 31:331–338, 1971.

import Control.Monad.Identity
import Data.Monoid

import Linear.Vector

import Data.Color
import Data.Turtle
import Data.LSystem

import Utility.SVGRender

data Plant c m a = PlantIndexed
	{ postProcess :: a -> [a]
	, lsystem :: LSystem m a
	, seed :: [a]
	, zero :: c
	, index :: c -> SVGDrawData
	}

data Honda = Axis Float Float | ApexA Float Float | ApexB Float Float

hondaSeed :: [TurtleSymbol Float Honda]
hondaSeed = [A $ Axis 1 10]

data HondaParams = HP
	{ trunkContraction :: Float
	, branchContraction :: Float
	, trunkBranchAngle :: Float
	, lateralBranchAngle :: Float
	, divergenceAngle :: Float
	, widthDecreaseRate :: Float
	}

defaultHonda :: HondaParams
defaultHonda = HP 0.9 0.6 (deg2rad 45) (deg2rad 45) (deg2rad 137.5) 0.707

{-
some basic expansion rules:


general themes on a[b]c[!b]d:
	F -> F[+F]F[-F]F
	F -> FF-[-F+F+F]+[+F-F-F]

	F -> FF
	X -> F[+X]F[-X]+X
	X -> F[+X][-X]FX
	X -> F-[X+X]+F[+FX]-X

basic random choice:
	s → s[//&&l][//∧∧l]Fs
	s → sFs
	s → s

-}


hondaAdvance :: HondaParams -> TurtleSymbol Float Honda -> [TurtleSymbol Float Honda]
hondaAdvance (HP r1 r2 a0 a2 d wr) h = case h of
	A (Axis l w) ->
		[ TA $ Color w, TA $ Advance l, TA Push
		, TA $ Pitch (-a0), A $ ApexA (l*r2) (w*wr)
		, TA Pop, TA $ Roll (-d)
		, A $ Axis (l*r1) (w*wr)
		]
	A (ApexA l w) ->
		[ TA $ Color w, TA $ Advance l, TA Push
		, TA $ Turn (-a2), TA $ CustomRotation id, A $ ApexB (l*r2) (w*wr)
		, TA Pop
		, A $ ApexB (l*r1) (w*wr)
		]
	A (ApexB l w) ->
		[ TA $ Color w, TA $ Advance l, TA Push
		, TA $ Turn a2, TA $ CustomRotation id, A $ ApexA (l*r2) (w*wr)
		, TA Pop
		, A $ ApexA (l*r1) (w*wr)
		]
	TA x -> [TA x]

-- the `id` there should be the `$` rotation:
--   $ = left becomes normalize (global up * heading); up becomes heading * left
-- (i don't know what that does)



data TernaryParams = TernP
	{ divergence1 :: Float
	, divergence2 :: Float
	, branchingAngle :: Float
	, elongationRate :: Float
	, widthIncreaseRate :: Float
	}

defaultTernary :: TernaryParams
defaultTernary = TernP 94.84 132.63 18.95 1.109 1.732

data Ternary = Ap


-- ω : !(1) F(200) /(45) A
defaultTernarySeed :: [TurtleSymbol Float Ternary]
defaultTernarySeed =
	[ TA $ Color 1
	, TA $ Advance 200
	, TA $ Roll 45
	, A Ap
	]

{-
p1: A : * → !(vr )F(50)[&(a)F(50)A]/(d1 )[&(a)F(50)A]/(d2 )[&(a)F(50)A]
p2: F(l) : * → F(l*lr )
p3: !(w) : * → !(w*vr )
-}
ternaryAdvance :: TernaryParams -> TurtleSymbol Float Ternary -> [TurtleSymbol Float Ternary]
ternaryAdvance (TernP d1 d2 a lr vr) t = case t of
	A Ap ->
		let
			pushSegment =
				[ TA Push
				, TA $ Pitch a
				, TA $ Advance 50
				, A Ap
				, TA Pop
				]
		in
			[ TA $ Color (vr)
			, TA $ Advance 50
			] <> pushSegment <>
			[ TA $ Roll d1
			] <> pushSegment <>
			[ TA $ Roll d2
			] <> pushSegment
	TA (Advance l) -> [TA $ Advance $ l * lr]
	TA (Color w) -> [TA $ Color $ w * vr]
	TA x -> [TA x]

{-
!(w) = set line width
F(x) = draw line forward x units
[ = push
] = pop
+ Turn left by angle δ, using rotation matrix R U (δ).
− Turn right by angle δ, using rotation matrix R U (−δ).
& Pitch down by angle δ, using rotation matrix R L (δ).
∧ Pitch up by angle δ, using rotation matrix R L (−δ).
\ Roll left by angle δ, using rotation matrix R H (δ).
/ Roll right by angle δ, using rotation matrix R H (−δ).
| Turn around, using rotation matrix R U (180 ◦ ).



Symbol Interpretation Page
F Move forward and draw a line. 7, 46
f Move forward without drawing a line. 7, 46
+ Turn left.
− Turn right.
∧ Pitch up. 19, 46
& Pitch down. 19, 46
\ Roll left. 19, 46
/ Roll right. 19, 46
| Turn around. 19, 46
$ Rotate the turtle to vertical. 57
[ Start a branch. 24
] Complete a branch. 24
{ Start a polygon.
G Move forward and draw a line. Do not record a vertex.
. Record a vertex in the current polygon. 122, 127
} Complete a polygon. 120, 127
∼ Incorporate a predefined surface.
! Decrement the diameter of segments.
' Increment the current color index. 26
% Cut off the remainder of the branch. 73
-}


{-
a flower:
	p -> i+[p+f]--//[--l]i[++l]-[pf]++pf
	i -> Fs[//&&l][//^^l]Fs
	s -> sFs
	l -> ['{+f-ff-f+|+f-ff-f}]
	f -> [&&&p'/w////w////w////w////w]
	p -> FF
	w -> ['^F][{&&&-f+f|-f+f}]
-}
data Flowering
	= Pl Int -- Plant
	| In Int -- Internode
	| Seg -- Segment
	| Le Int -- Leaf
	| Fl -- Flower
	| Ped -- Pedicel
	| We -- Wedge

data FloweringParams = FP
	{ angle :: Float
	, petals :: Int
	}

floweringDefaults :: FloweringParams
floweringDefaults = FP (deg2rad 18) 5

floweringSeed :: [TurtleSymbol Int Flowering]
floweringSeed = [A $ Pl 0]

floweringAdvance :: FloweringParams -> TurtleSymbol Int Flowering -> [TurtleSymbol Int Flowering]
floweringAdvance (FP δ ps) t = case t of
	A (Pl i) -> -- p -> i+[p+f]--//[--l]i[++l]-[pf]++pf
		[ A (In i), s_add, TA Push, A (Pl i), s_add, A Fl, TA Pop, s_sub2, roll
		, TA Push, s_sub2, A (Le i), TA Pop, A (In i)
		, TA Push, s_add2, A (Le i), TA Pop, s_sub
		, TA Push, A (Pl $ i + 1), A Fl, TA Pop, s_add2, A (Pl $ i + 1), A Fl
		]
	A (In i) -> -- i -> Fs[//&&l][//^^l]Fs
		[ TA $ Advance 1, A Seg
		, TA Push, roll, pitch_down, A (Le i), TA Pop
		, TA Push, roll, pitch_up, A (Le i), TA Pop
		, TA $ Advance 1, A Seg
		]
	A Seg -> -- s -> sFs
		[ A Seg, TA $ Advance 1, A Seg
		]
	A (Le i) -> -- l -> ['{+f-ff-f+|+f-ff-f}]
		[ TA Push
		, TA $ MutColor (\x -> x+3+i)
		, TA $ DrawMode Polygon
		, s_add, TA $ Advance 1, s_sub, TA $ Advance 2, s_sub, TA $ Advance 1
		, s_add, turnAround
		, s_add, TA $ Advance 1, s_sub, TA $ Advance 2, s_sub, TA $ Advance 1
		, TA $ DrawMode Record
		, TA Pop
		]
	A Fl -> -- f -> [&&&p'/w////w////w////w////w]
		[ TA Push
		, pitch_down3, A Ped, TA $ MutColor (\i -> i+1)
		, roll1
		] <> concat (replicate ps [A We, petalRoll]) <>
		[ TA Pop
		]
	A Ped -> -- p -> FF
		[ TA $ Advance 2
		]
	A We -> -- w -> ['^F][{&&&-f+f|-f+f}]
		[ TA Push, TA $ MutColor (\i -> i+1), pitch_up1, TA $ Advance 1, TA Pop
		, TA Push, TA $ DrawMode Polygon
		, pitch_down3
		, s_sub, TA $ Advance 1, s_add, TA $ Advance 1, turnAround
		, s_sub, TA $ Advance 1, s_add, TA $ Advance 1
		, TA $ DrawMode Record
		, TA Pop
		]
	TA x -> [TA x]
	where
		petalRoll = TA $ Roll $ deg2rad $ 360 / fromIntegral ps
		s_sub = TA $ Turn (-δ)
		s_sub2 = TA $ Turn (-δ * 2)
		s_add = TA $ Turn δ
		s_add2 = TA $ Turn (δ * 2)
		roll = TA $ Roll (δ * 2)
		pitch_down = TA $ Pitch (δ * 2)
		pitch_down3 = TA $ Pitch (δ * 3)
		pitch_up = TA $ Pitch $ (-δ) * 2
		pitch_up1 = TA $ Pitch (-δ)
		roll1 = TA $ Roll δ
		roll4 = TA $ Roll (δ * 4)
		turnAround = TA $ Turn pi -- CustomRotation $ (^* (-1))

floweringColorIndex :: Int -> SVGDrawData
floweringColorIndex i = case i of
	0 -> stroke (RGB8 0x22 0x44 0x11) <> strokeWidth 0.1
	1 -> fill (RGB8 0xff 0xff 0xff) <> strokeWidth 0.1
	2 -> stroke (RGB8 0xff 0x22 0x00) <> strokeWidth 0.05
	3 -> fill (RGB8 0x00 0x44 0x00)
	4 -> fill (RGB8 0x08 0x55 0x08)
	5 -> fill (RGB8 0x10 0x66 0x10)
	6 -> fill (RGB8 0x16 0x77 0x16)
	7 -> fill $ RGB8 0x1a 0x88 0x1a
	8 -> fill $ RGB8 0x1d 0x99 0x1d
	_ -> stroke (RGB8 0xff 0x00 0xff) <> fill (RGB8 0xff 0x00 0xff)

floweringPlant :: FloweringParams -> Plant Int Identity (TurtleSymbol Int Flowering)
floweringPlant params = PlantIndexed
	return
	(dcfLSystem $ floweringAdvance params)
	floweringSeed
	0
	floweringColorIndex

{-
a bush:
	A -> [&FL!A]/////'[&FL!A]///////'[&FL!A]
	F -> S/////F
	S -> FL
	L -> ['''^^{ -f+f+f-|-f+f+f }]
-}


data Bush = Apex | BushS | BushL
	deriving (Eq, Ord, Show, Read)

bushSeed :: [TurtleSymbol (Int, Int) Bush]
bushSeed = [A Apex]

bushAdvance :: TurtleSymbol (Int, Int) Bush -> [TurtleSymbol (Int, Int) Bush]
bushAdvance sym = case sym of
	A Apex -> -- A -> [&FL!A]/////'[&FL!A]///////'[&FL!A]
		let
			branch :: [TurtleSymbol (Int, Int) Bush]
			branch = [TA Push, down, TA $ Advance 1.01, A BushL, TA $ MutColor (\(width, index) -> (width-1, index)), A Apex, TA Pop]
		in
			branch <>
			[ roll5, TA $ MutColor (\(width, index) -> (width, index+1)) ] <>
			branch <>
			[ roll7, TA $ MutColor (\(width, index) -> (width, index+1)) ] <>
			branch
	TA (Advance 1.01) -> -- F -> S/////F
		[ A BushS, roll5, TA $ Advance 1.01
		]
	A BushS -> -- S -> FL
		[ TA $ Advance 1.01, A BushL
		]
	A BushL -> -- L -> ['''^^{ -f+f+f-|-f+f+f }]
		[ TA Push
		, TA $ MutColor (\(width, index) -> (0, index+3)), up2
		, TA $ DrawMode Polygon
		, left, TA $ Advance 1, right, TA $ Advance 1, right, TA $ Advance 1, left, turn
		, left, TA $ Advance 1, right, TA $ Advance 1, right, TA $ Advance 1
		, TA $ DrawMode Record
		, TA Pop
		]
	TA x -> [TA x]
	where
		δ = deg2rad 22.5
		roll5 = TA $ Roll (δ * 5)
		roll7 = TA $ Roll (δ * 7)
		up2 = TA $ Pitch (-δ * 2)
		down = TA $ Pitch δ
		left = TA $ Turn δ
		right = TA $ Turn (-δ)
		turn = TA $ Turn pi

bushColorIndex :: (Int, Int) -> SVGDrawData
bushColorIndex (width, color) =
	(if width == 0
		then mempty
		else strokeWidth (fromIntegral width / 30))
	<> case color of
		0 -> act $ RGB8 0x00 0x00 0x00
		1 -> act $ RGB8 0x22 0x22 0x11
		2 -> act $ RGB8 0x44 0x44 0x22
		3 -> act $ RGB8 0x66 0x66 0x33
		4 -> act $ RGB8 0x88 0x88 0x44
		5 -> act $ RGB8 0xaa 0xaa 0x55
		6 -> act $ RGB8 0xcc 0xcc 0x66
		7 -> act $ RGB8 0xee 0xee 0x77
		8 -> act $ RGB8 0xff 0xff 0x78
		9 -> act $ RGB8 0xf8 0xf8 0xff
		10 -> act $ RGB8 0xee 0xee 0xff
		11 -> act $ RGB8 0xdd 0xdd 0xff
		12 -> act $ RGB8 0xcc 0xcc 0xff
		13 -> act $ RGB8 0xbb 0xaa 0xff
		14 -> act $ RGB8 0xaa 0x99 0xff
		15 -> act $ RGB8 0x99 0x77 0xff
		_ -> act $ RGB8 0xff 0x00 0xff
	where
		act = if width == 0 then fill else stroke

bush = PlantIndexed
	return
	(dcfLSystem bushAdvance)
	bushSeed
	(5, 0)
	bushColorIndex


data HogewegHesper = HH
	{ hAngle :: Float
	, initial :: (Bool, Bool, Bool)
	, minorGrowth :: (Bool, Bool, Bool)
	, majorGrowth :: (Bool, Bool, Bool)
	, switchFunc :: (Bool, Bool, Bool) -> Bool
	}

hogewegHesperA = HH (deg2rad 22.5) (True, True, True)
	(True, False, True)
	(False, False, True)
	$ \(pred, cur, succ) -> case (pred, cur, succ) of
		(False, False, False) -> False
		(False, False, True) -> False -- major
		(False, True, False) -> True
		(False, True, True) -> True
		(True, False, False) -> False
		(True, False, True) -> False -- minor
		(True, True, False) -> False
		(True, True, True) -> False

hogewegHesperB = HH (deg2rad 22.5) (True, True, True)
	(True, False, True)
	(False, False, True)
	$ \(pred, cur, succ) -> case (pred, cur, succ) of
		(False, False, False) -> True
		(False, False, True) -> False -- major
		(False, True, False) -> True
		(False, True, True) -> True
		(True, False, False) -> False
		(True, False, True) -> False -- minor
		(True, True, False) -> True
		(True, True, True) -> False

hogewegHesperC = HH (deg2rad 25.75) (True, True, True)
	(True, False, True)
	(False, True, True)
	$ \(pred, cur, succ) -> case (pred, cur, succ) of
		(False, False, False) -> False
		(False, False, True) -> True
		(False, True, False) -> False
		(False, True, True) -> True -- major
		(True, False, False) -> False
		(True, False, True) -> False -- minor
		(True, True, False) -> False
		(True, True, True) -> False

hogewegHesperD = HH (deg2rad 25.75) (False, True, True)
	(False, True, True)
	(True, False, True)
	$ \(pred, cur, succ) -> case (pred, cur, succ) of
		(False, False, False) -> True
		(False, False, True) -> False
		(False, True, False) -> False
		(False, True, True) -> True -- minor
		(True, False, False) -> True
		(True, False, True) -> False -- major
		(True, True, False) -> True
		(True, True, True) -> False

hogewegHesperE = HH (deg2rad 22.5) (True, True, True)
	(True, False, True)
	(False, False, True)
	$ \(pred, cur, succ) -> case (pred, cur, succ) of
		(False, False, False) -> False
		(False, False, True) -> False -- major
		(False, True, False) -> True
		(False, True, True) -> True
		(True, False, False) -> False
		(True, False, True) -> False -- minor
		(True, True, False) -> True
		(True, True, True) -> False

hogewegHesperSeed :: HogewegHesper -> [TurtleSymbol () Bool]
hogewegHesperSeed hh =
	[ TA $ Advance 1
	, A ta
	, TA $ Advance 1
	, A tb
	, TA $ Advance 1
	, A tc
	]
	where
		(ta, tb, tc) = initial hh

hogewegHesperAdvance :: HogewegHesper -> Maybe (TurtleSymbol () Bool) -> TurtleSymbol () Bool -> Maybe (TurtleSymbol () Bool) -> [TurtleSymbol () Bool]
hogewegHesperAdvance hh pred cur succ = case (pred, cur, succ) of
	(Just (A ta), A tb, Just (A tc)) -> if (ta, tb, tc) == majorGrowth hh
		then [A True, TA Push, TA $ Turn (hAngle hh)
			, TA $ Advance 1, A True, TA $ Advance 1, A True
			, TA Pop
			]
		else if (ta, tb, tc) == minorGrowth hh
			then [A True, TA $ Advance 1, A True]
			else pure $ A $ switchFunc hh (ta, tb, tc)
	(_, TA (Turn x), _) -> [TA $ Turn (-x)]
	(_, x, _) -> [x]

hogewegHesperLSystem :: HogewegHesper -> LSystem Identity (TurtleSymbol () Bool)
hogewegHesperLSystem hh =
	turtlePreLSystem (\sym -> case sym of
		TA (Advance _) -> True
		TA (Turn _) -> True
		_ -> False) $ hogewegHesperAdvance hh


-- G should be interpreted exactly as TA Advance 1
-- i have no clue how to color these tho
data MycelisMuralisIII
	= I Int | MApex Int
	| L | T | V | M | W
	| F | G
	| K Int
	deriving (Eq, Ord, Show, Read)
-- ignore every symbol save for M (M)S T V

-- | X Florigen
{-data Florigen
	= I Int -- root terminus counter
	| M -- base state
	| L -- stop signal, sent from base to apex
	| T -- lifting signal, sent from apex to base
	| W | V -- branch stop signal, sent from apex to base at half speed
-}

mycelisMuralisIIIPostProcess :: TurtleSymbol (Bool, Int) MycelisMuralisIII -> [TurtleSymbol (Bool, Int) MycelisMuralisIII]
mycelisMuralisIIIPostProcess (A F) = [TA $ Advance 1]
mycelisMuralisIIIPostProcess (A G) = [TA $ Advance 1]
mycelisMuralisIIIPostProcess (A M) =
	[TA $ MutColor (const (True, 0))] -- <> poly 4 0.33 (False, 0)
mycelisMuralisIIIPostProcess (A L) =
	[TA $ MutColor (const (True, 1))] -- <> poly 4 0.33 (False, 1)
mycelisMuralisIIIPostProcess (A T) =
	[TA $ MutColor (const (True, 2))] -- <> poly 4 0.33 (False, 2)
mycelisMuralisIIIPostProcess (A W) =
	[TA $ MutColor (const (True, 3))] -- <> poly 4 0.66 (False, 3)
mycelisMuralisIIIPostProcess (A V) =
	[TA $ MutColor (const (True, 4))] -- <> poly 4 0.33 (False, 4)
mycelisMuralisIIIPostProcess (TA Push) =
	[TA Push, TA $ MutColor (const (True, 0))]

mycelisMuralisIIIPostProcess (A (K 0)) = poly 8 0.08 (False, (-1))
mycelisMuralisIIIPostProcess (A (K 1)) = poly 8 0.16 (False, 0)
mycelisMuralisIIIPostProcess (A (K 2)) = poly 8 0.16 (False, 2)

mycelisMuralisIIIPostProcess (A (K 3)) = spike 6 0.5 (False, 5)
mycelisMuralisIIIPostProcess (A (K 4)) = spike 6 0.5 (False, 6)
mycelisMuralisIIIPostProcess (A (K 5)) = spike 6 0.5 (False, 7)

mycelisMuralisIIIPostProcess (A (K 6)) = diamond 0.5 (False, 8)
mycelisMuralisIIIPostProcess (A (K 7)) = diamond 0.5 (False, 9)
mycelisMuralisIIIPostProcess (A (K 8)) = diamond 0.5 (False, 10)
mycelisMuralisIIIPostProcess (A (K 9)) = diamond 0.5 (False, 11)

mycelisMuralisIIIPostProcess (A (K 10)) = poly 3 0.66 (False, 8)
mycelisMuralisIIIPostProcess (A (K 11)) = poly 3 0.66 (False, 9)
mycelisMuralisIIIPostProcess (A (K 12)) = poly 3 0.66 (False, 10)
mycelisMuralisIIIPostProcess (A (K 13)) = poly 3 0.66 (False, 11)
mycelisMuralisIIIPostProcess (A (K i)) = poly 3 0.50 (False, 11)

mycelisMuralisIIIPostProcess x = [x]

poly l s i =
	[ TA Push
	, TA $ MutColor (const i)
	, TA $ DrawMode Polygon
	-- , TA $ Turn (angle * (0.5))
	] <>
	concat (replicate l [TA $ Advance s, TA $ Turn angle]) <>
	[ TA Pop
	]
	where
		angle = deg2rad $ 360 / fromIntegral l

spike l s i =
	[ TA Push
	] <> concat (replicate l $
		[ TA Push
		, TA $ DrawMode Ghost
		, TA $ Advance $ s * 0.33
		, TA $ MutColor $ const i
		, TA $ DrawMode Polygon
		] <> diamondDraws s <>
		[ TA Pop
		, TA $ Turn a
		])
	<> [ TA Pop ]
	where
		a = deg2rad $ 360 / fromIntegral l

diamond s i =
	[ TA Push
	, TA $ MutColor $ const i
	, TA $ DrawMode Polygon
	] <> diamondDraws s
	<> [ TA Pop ]

diamondDraws s =
	[ TA $ Turn $ deg2rad (30)
	, TA $ Advance s
	, TA $ Turn $ deg2rad (-60)
	, TA $ Advance s
	, TA $ Turn $ deg2rad (-120)
	, TA $ Advance s
	, TA $ Turn $ deg2rad (-60)
	, TA $ Advance s
	]

mycelisMuralisIIILSystem :: LSystem Identity (TurtleSymbol (Bool, Int) MycelisMuralisIII)
mycelisMuralisIIILSystem = turtlePreLSystem (\sym -> case sym of
		A x | x `elem` [M, L, T, V, W] -> False
		_ -> True) $ mycelisMuralisIIIAdvance

mycelisMuralisIIIAdvance :: Maybe (TurtleSymbol (Bool, Int) MycelisMuralisIII) -> TurtleSymbol (Bool, Int) MycelisMuralisIII -> Maybe (TurtleSymbol (Bool, Int) MycelisMuralisIII) -> [TurtleSymbol (Bool, Int) MycelisMuralisIII]
mycelisMuralisIIIAdvance pred sym succ = case (pred, sym, succ) of
	(Just (A L) , A (MApex t), _) -> [A T, A V, A $ K 0]
	(Just (A V) , A (MApex t), _) -> [A T, A V, A $ K 0]
	(_          , A (MApex t), _)
		| t > 0 -> [A $ MApex (t-1)]
		| t == 0 -> [A M, TA Push, right, A G, TA Pop, A F, flip, A $ MApex 2]
	(Just (A L) , A M        , _) -> [A L]
	(_          , A L        , Just (A T)) -> [A T]
	(Just (A T) , A G        , _) -> [A F, A $ MApex 2]
	(Just (A V) , A M        , _) -> [A L]
	(_          , A T        , Just (A V)) -> [A W]
	(_          , A W        , _) -> [A V]
	(_          , A (I t)    , _)
		| t > 0 -> [A $ I (t-1)]
		| t == 0 -> [A L]
	(_          , A (K i)    , _) -> [A $ K (i+1)]
	(_          , TA x       , _) -> [TA x]
	(_          , A x        , _) -> [A x]
	where
		right = TA $ Turn (deg2rad 30)
		flip = TA $ Roll (deg2rad 180)
{-
#include K
#consider M S T V
 ω  : I(20)FA(0)

 p1 : S < A(t)  : * -> TVK
 p2 : V < A(t)  : * -> TVK
 p3 :     A(t)  : t>0 -> A(t-1)
 p4 :     A(t)  : t=0 -> M[+(30)G]F/(180)A(2)
 p5 : S < M     : * -> S
 p6 :     S > T : * -> T
 p7 : T < G     : * -> FA(2)
 p8 : V < M     : * -> S
 p9 :     T > V : * -> W
p10 :     W     : * -> V
p11 :     I(t)  : t>0 -> I(t-1)
p12 :     I(t)  : t=0 -> S
-}
mycelisMuralisIIISeed :: [TurtleSymbol a MycelisMuralisIII]
mycelisMuralisIIISeed = [A $ I 20, TA $ Advance 1, A $ MApex 0]

mycelisMuralisIIIColorIndex :: (Bool, Int) -> SVGDrawData
mycelisMuralisIIIColorIndex (s, i) = (case i of
	-1 -> act $ RGB8 0xee 0xee 0xff
	0 -> act $ RGB8 0x99 0xdd 0x44
	1 -> act $ RGB8 0x88 0x00 0x88
	2 -> act $ RGB8 0x33 0x88 0x11
	3 -> act $ RGB8 0x88 0x00 0x00
	4 -> act $ RGB8 0x00 0x00 0x88
	5 -> act $ RGB8 0xbb 0xff 0x55
	6 -> act $ RGB8 0xdd 0xdd 0x44
	7 -> act $ RGB8 0xaa 0xaa 0x22
	8 -> act $ RGB8 0x88 0x77 0x11
	9 -> act $ RGB8 0x77 0x66 0x11
	10 -> act $ RGB8 0x66 0x55 0x00
	11 -> act $ RGB8 0x55 0x33 0x00
	_ -> act $ RGB8 0x22 0xee 0xee)
	where
		act c = if s
			then stroke c <> fill c <> strokeWidth 0.1
			else fill c

mycelisMuralisIII = PlantIndexed
	mycelisMuralisIIIPostProcess
	mycelisMuralisIIILSystem
	mycelisMuralisIIISeed
	(True, 0)
	mycelisMuralisIIIColorIndex

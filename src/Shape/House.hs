module Shape.House where

import Control.Applicative
import Data.Monoid
import Data.Maybe

import Linear.V2
import Linear.V3

import Data.PossibilitySpace

import Data.Color
import Data.Turtle
import Geometry.Quad

import Shape

data House = House
	{ _hy :: Int
	, _hx :: Int
	, _hz :: Int
	, _style :: BuildingStyle
	, _roof :: RoofSlope
	, _decorations :: [Decoration]
	}
	deriving (Eq, Ord, Show, Read)

houseSpace :: RenderSpace House Surface ColorRGB
houseSpace = RenderSpace
	(complexHouses (6, 8) (5, 10) (5, 10))
	(renderHouse (-3.5) (-3.5) (\_ _ -> 0))
	(const surfaceColor)

footprint :: House -> V2 Int
footprint (House _ x z _ _ _) = V2 x z

data BuildingStyle = Full Surface | Stilts Float
	deriving (Eq, Ord, Show, Read)

data RoofSlope = Flat | Skillion Float | Pitched
	deriving (Eq, Ord, Show, Read)

data Decoration = DA Antenna | DF Flag | DH StackedHouse | DS SideHouse | DW RoofWall
	deriving (Eq, Ord, Show, Read)

data Antenna = Stick (V2 Int) Int | Cross (V2 Int) Int Int | Net (V2 Int) Int Int Int
	deriving (Eq, Ord, Show, Read)

antennaBase :: Antenna -> V2 Int
antennaBase (Stick b _) = b
antennaBase (Cross b _ _) = b
antennaBase (Net b _ _ _) = b

data Flag = Flag (V2 Int) Int Int FlagShape Int
	deriving (Eq, Ord, Show, Read)

flagBase :: Flag -> V2 Int
flagBase (Flag b _ _ _ _) = b

data FlagShape = FlagTri | FlagQuad
	deriving (Eq, Ord, Show, Read)

data RoofWall = RoofWall Float Int
	deriving (Eq, Ord, Show, Read)

data StackedHouse = Stacked (V2 Int) House
	deriving (Eq, Ord, Show, Read)
data SideHouse = Side (V2 Int) House
	deriving (Eq, Ord, Show, Read)

data Surface = SFloor | SWall | SRoof | SWire | SCanvas | SWood
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

surfaceColor :: Surface -> ColorRGB
surfaceColor s = case s of
	SFloor -> RGB8 0x88 0x66 0x55
	SWall -> RGB8 0xee 0xdd 0xcc
	SRoof -> RGB8 0xdd 0xaa 0x99
	SWire -> RGB8 0x10 0x10 0x20
	SCanvas -> RGB8 0xcc 0xdd 0xee
	SWood -> RGB8 0x80 0x50 0x28

simpleHouses :: (Int, Int) -> (Int, Int) -> (Int, Int) -> PossibilitySpace House
simpleHouses y w h = House
	<$> rangeEnum y
	<*> rangeEnum w
	<*> rangeEnum h
	<*> pure (Full SWall)
	<*> from [Flat, Skillion 1, Skillion (-1), Pitched]
	<*> pure []

decoratedHouses :: (Int, Int) -> (Int, Int) -> (Int, Int) -> PossibilitySpace House
decoratedHouses y x z = addDecorations =<< simpleHouses y x z
	where
		addDecorations :: House -> PossibilitySpace House
		addDecorations h =
			let decsSet = drawsOfNRange (0, 5) $ decorations (_hx h) (_hz h)
			in (\decs -> h { _decorations = decs <> _decorations h }) <$> decsSet

decorations :: Int -> Int -> PossibilitySpace Decoration
decorations x z =
	(DA <$> antennas x z)
	<|> (DF <$> flags x z)
	<|> (DW <$> RoofWall 0 <$> rangeEnum (1, 2))

flags :: Int -> Int -> PossibilitySpace Flag
flags x z =
	Flag <$> (V2 <$> rangeEnum (0, x) <*> rangeEnum (0, z))
		<*> rangeEnum (3, 6) -- height of flag pole
		<*> rangeEnum (1, 2) -- backwards expanse of flag pole
		<*> from [FlagTri, FlagQuad] -- flag shape
		<*> rangeEnum (1, 3) -- flag length

antennas :: Int -> Int -> PossibilitySpace Antenna
antennas x z =
	(Stick
		<$> (V2 <$> rangeEnum (0, x) <*> rangeEnum (0, z))
		<*> rangeEnum (1, 4))
	<|> (Cross
		<$> (V2 <$> rangeEnum (0, x) <*> rangeEnum (0, z))
		<*> rangeEnum (2, 4)
		<*> rangeEnum (1, 2))
	<|> (Net
		<$> (V2 <$> rangeEnum (0, x) <*> rangeEnum (0, z))
		<*> rangeEnum (2, 6)
		<*> pure 1
		<*> rangeEnum (1, 6))


complexHouses :: (Int, Int) -> (Int, Int) -> (Int, Int) -> PossibilitySpace House
complexHouses y x z =
	decoratedHouses y x z
	<|> (attachToSide
		<$> decoratedHouses y x z
		<*> (from [0,1,2,3])
		<*> decoratedHouses y x z)

{-
complexHouses :: (Int, Int) -> (Int, Int) -> PossibilitySpace House
complexHouses w h =
	drawsOfNRange (0, 1) lside decoratedHouses w h
	where
		attachments :: PossibilitySpace (House -> House)
		attachments = mconcat
			[ drawsOfNRange (0, 1) lside
			, drawsOfNRange (0, 1) rside
			, drawsOfNRange (0, 1) tside
			, drawsOfNRange (0, 1) bside
			, drawsOfNRange (0, 1) tops
			]
		lside = attachToSide
			<$> decoratedHouses w h
			<*> pure (V2 False False)
		rside = attachToSide
			<$> decoratedHouses w h
			<*> pure (V2 False True)
		tside = attachToSide
			<$> decoratedHouses w h
			<*> pure (V2 True False)
		bside = attachToSide
			<$> decoratedHouses w h
			<*> pure (V2 True True)
		tops = attachToTop
			<$> decoratedHouses w h
			<*> (V2 <$> rangeEnum w <*> rangeEnum h))

(foo =<< decoratedHouses (5, 10) (5, 10))
	where
		foo h = let
				x = _hx h
				z = _hz h
			in
				mconcat
					[ pure h
					, attachToSide <$> (V2 <$> from [True, False] <*> [True, False]) <*> decoratedHouses (3, x) (3, z)
					]

				(\d -> h { _decorations = d : _decorations h) <$> decorations x z) =<< simpleHouses
-}

toStilt :: House -> House
toStilt h = h { _style = Stilts 0.5 }

attachToSide :: House -> Int -> House -> House
attachToSide side o h =
	h { _decorations = DS (Side (V2 xOffset zOffset) side) : _decorations h }
	where
		(xOffset, zOffset) = case o of
			0 -> (_hx h, 0)
			1 -> (0, _hz h)
			2 -> (0 - _hx side, 0)
			3 -> (0 - _hz side, 0)
			_ -> error "bad side"

attachToTop :: House -> V2 Int -> House -> House
attachToTop stack offset h =
	h { _decorations = DH (Stacked offset stack) : _decorations h }

renderHouse :: Float -> Float -> (Float -> Float -> Float) -> House -> [TRecord Surface]
renderHouse xDisplace zDisplace groundLevel h@(House y x' z' surface roof decs) = wallPoly <> roofPoly <> decorationPoly
	where
		rz, rx :: Float
		rz = fromIntegral z'
		rx = fromIntegral x'
		bx = xDisplace
		mx = xDisplace + (rx / 2)
		x = xDisplace + rx
		bz = zDisplace
		mz = zDisplace + (rz / 2)
		z = zDisplace + rz
		baseFloor = minimum [floorLevel 0 0, floorLevel 0 1, floorLevel 1 1, floorLevel 1 0]
		floorLevel fx fz = groundLevel (fx * rx) (fz * rz)
		ceilingLevel cx cz = baseFloor + roofHeightAt h (V2 (cx * rx) (cz * rz))
		wallPoly = case surface of
			Stilts thickness ->
				let
					stiltAt :: Float -> Float -> [TRecord Surface]
					stiltAt sx sz = translateRecord (V3 (sx + bx) 0 (sz + bz))
						<$> rectColumn
							thickness thickness
							groundLevel
							(\x z -> (baseFloor +) . roofHeightAt h $ V2 (sx + x) (sz + z))
							SWood
				in
					mconcat $ stiltAt <$> [0, rx-thickness] <*> [0, rz-thickness]
			Full s ->
				([ TPoly $ Convex
					[ TP (V3 bx (floorLevel 0 0) bz) SFloor
					, TP (V3 x (floorLevel 1 0) bz) SFloor
					, TP (V3 x (floorLevel 1 1) z) SFloor
					, TP (V3 bx (floorLevel 0 1) z) SFloor
					]

				, TPoly $ Convex
					[ TP (V3 bx (floorLevel 0 0) bz) s
					, TP (V3 x (floorLevel 1 0) bz) s
					, TP (V3 x (ceilingLevel 1 0) bz) s
					, TP (V3 bx (ceilingLevel 0 0) bz) s
					]
				, TPoly $ Convex
					[ TP (V3 bx (floorLevel 0 1) z) s
					, TP (V3 bx (ceilingLevel 0 1) z) s
					, TP (V3 x (ceilingLevel 1 1) z) s
					, TP (V3 x (floorLevel 1 1) z) s
					]
				] <> case roof of
					Pitched ->
						[ TPoly $ Convex
							[ TP (V3 bx (floorLevel 0 0) bz) s
							, TP (V3 bx (floorLevel 0 1) z) s
							, TP (V3 bx (ceilingLevel 0 1) z) s
							, TP (V3 bx (ceilingLevel 0 0.5) mz) s
							, TP (V3 bx (ceilingLevel 0 0) bz) s
							]
						, TPoly $ Convex
							[ TP (V3 x (floorLevel 1 1) z) s
							, TP (V3 x (floorLevel 1 0) bz) s
							, TP (V3 x (ceilingLevel 1 0) bz) s
							, TP (V3 x (ceilingLevel 1 0.5) mz) s
							, TP (V3 x (ceilingLevel 1 1) z) s
							]
						]
					_ ->
						[ TPoly $ Convex
							[ TP (V3 bx (floorLevel 0 0) bz) s
							, TP (V3 bx (floorLevel 0 1) z) s
							, TP (V3 bx (ceilingLevel 0 1) z) s
							, TP (V3 bx (ceilingLevel 0 0) bz) s
							]
						, TPoly $ Convex
							[ TP (V3 x (floorLevel 1 1) z) s
							, TP (V3 x (floorLevel 1 0) bz) s
							, TP (V3 x (ceilingLevel 1 0) bz) s
							, TP (V3 x (ceilingLevel 1 1) z) s
							]
						])
		roofPoly = case roof of
			Pitched ->
				[ TPoly $ Convex
					[ TP (V3 bx (ceilingLevel 0 0) bz) SRoof
					, TP (V3 x (ceilingLevel 1 0) bz) SRoof
					, TP (V3 x (ceilingLevel 1 0.5) mz) SRoof
					, TP (V3 bx (ceilingLevel 0 0.5) mz) SRoof
					]
				, TPoly $ Convex
					[ TP (V3 bx (ceilingLevel 0 0.5) mz) SRoof
					, TP (V3 x (ceilingLevel 1 0.5) mz) SRoof
					, TP (V3 x (ceilingLevel 1 1) z) SRoof
					, TP (V3 bx (ceilingLevel 0 1) z) SRoof
					]
				]
			_ -> roofPolys (V3 bx baseFloor bz) h
		decorationPoly = decoration (V3 xDisplace baseFloor zDisplace) h =<< decs


rectColumn :: Float -> Float -> (Float -> Float -> Float) -> (Float -> Float -> Float) -> Surface -> [TRecord Surface]
rectColumn w l bottom top s =
	[ TPoly $ Convex
		[ TP (V3 0 (bottom 0 0) 0) s
		, TP (V3 w (bottom w 0) 0) s
		, TP (V3 w (top    w 0) 0) s
		, TP (V3 0 (top    0 0) 0) s
		]
	, TPoly $ Convex
		[ TP (V3 w (bottom w l) l) s
		, TP (V3 0 (bottom 0 l) l) s
		, TP (V3 0 (top    0 l) l) s
		, TP (V3 w (top    w l) l) s
		]
	, TPoly $ Convex
		[ TP (V3 0 (bottom 0 0) 0) s
		, TP (V3 0 (bottom 0 l) l) s
		, TP (V3 0 (top    0 l) l) s
		, TP (V3 0 (top    0 0) 0) s
		]
	, TPoly $ Convex
		[ TP (V3 w (bottom w l) l) s
		, TP (V3 w (bottom w 0) 0) s
		, TP (V3 w (top    w 0) 0) s
		, TP (V3 w (top    w l) l) s
		]
	]

-- this won't work for pitched roofs, because it can't detect the inflection point of the roof slope
roofPolys :: V3 Float -> House -> [TRecord Surface]
roofPolys offset h = translateQuad . fmap fromIntegral <$> roofQuads h
	where
		translateQuad (Quad (V2 x z) (V2 w l)) =
			TPoly $ Convex
				[ TP (offset + V3  x    (ceilingLevel  x     z   )  z   ) surface
				, TP (offset + V3 (x+w) (ceilingLevel (x+w)  z   )  z   ) surface
				, TP (offset + V3 (x+w) (ceilingLevel (x+w) (z+l)) (z+l)) surface
				, TP (offset + V3  x    (ceilingLevel  x    (z+l)) (z+l)) surface
				]
		surface = case _roof h of
			Flat -> SWall
			_ -> SRoof
		ceilingLevel cx cz = roofHeightAt h (V2 cx cz)

roofQuads :: House -> [Quad Int]
roofQuads h@(House y x z _ r ds) = quads
	where
		quads :: [Quad Int]
		quads = fromMaybe [Quad 0 $ footprint h]
			$ (\q -> Quad 0 (footprint h) `quadLeftComplement` q) <$> foo ds
		foo [] = Nothing
		foo (DH (Stacked offset h'):_) = Just $ Quad offset (footprint h')
		foo (_:rs) = foo rs

roofHeightAt :: House -> V2 Float -> Float
roofHeightAt (House y' x z _ r _) (V2 xp zp) = slopeFunc zpercent
	where
		y, pitchHeight :: Float
		y = fromIntegral y'
		zpercent = zp / fromIntegral z
		slopeFunc = case r of
			Flat -> const (-y)
			Skillion slope -> \p -> (-y) - (p * slope)
			Pitched -> \p -> if p < 0.5
				then negate $ y + (pitchHeight * p * 2)
				else negate $ (y + pitchHeight) - ((p - 0.5) * 2 * pitchHeight)
		pitchHeight = 2

roofVectorAt :: House -> V2 Float -> V3 Float
roofVectorAt h v@(V2 xp zp) = V3 xp (roofHeightAt h v) zp

decoration :: V3 Float -> House -> Decoration -> [TRecord Surface]
decoration displace h@(House _ x z _ _ _) (DA a) =
		let
			rx, rz :: Float
			V2 rx rz = fromIntegral <$> antennaBase a
			roofPos = displace + V3
				(rx)
				(roofHeightAt h $ V2 rx rz)
				(rz)
		in antenna a roofPos
decoration displace h@(House _ x z _ _ _) (DF f) =
		let
			rx, rz :: Float
			V2 rx rz = fromIntegral <$> flagBase f
			roofPos = displace + V3
				(rx)
				(roofHeightAt h $ V2 rx rz)
				(rz)
		in flag f roofPos
decoration displace h@(House y x' z' _ _ _) (DW (RoofWall _ height')) =
	let
		x, z, height :: Float
		x = fromIntegral x'
		z = fromIntegral z'
		height = fromIntegral height'
		fenceLine base = TLine
			(TP (displace + base) SWood)
			(TP (displace + base + V3 0 (-height) 0) SWood)
	in
		mconcat
			[ fenceLine . roofVectorAt h <$> (V2 0 <$> [0..z])
			, fenceLine . roofVectorAt h <$> (V2 x <$> [0..z])
			, fenceLine . roofVectorAt h <$> (V2 <$> [1..x-1] <*> pure 0)
			, fenceLine . roofVectorAt h <$> (V2 <$> [1..x-1] <*> pure z)
			]
		<>
			[ TLine
				(TP (displace + V3 0 (roofHeightAt h (V2 0 0) - height) 0) SWood)
				(TP (displace + V3 x (roofHeightAt h (V2 x 0) - height) 0) SWood)
			, TLine
				(TP (displace + V3 x (roofHeightAt h (V2 x 0) - height) 0) SWood)
				(TP (displace + V3 x (roofHeightAt h (V2 x z) - height) z) SWood)
			, TLine
				(TP (displace + V3 x (roofHeightAt h (V2 x z) - height) z) SWood)
				(TP (displace + V3 0 (roofHeightAt h (V2 0 z) - height) z) SWood)
			, TLine
				(TP (displace + V3 0 (roofHeightAt h (V2 0 z) - height) z) SWood)
				(TP (displace + V3 0 (roofHeightAt h (V2 0 0) - height) 0) SWood)
			]
decoration (V3 dx dy dz) h@(House _ x z _ _ _) (DH (Stacked (V2 xo zo) h')) =
	renderHouse (dx + fromIntegral xo) (dz + fromIntegral zo)
		(\x z -> dy + roofHeightAt h (V2 (fromIntegral xo + x) (fromIntegral zo + z)))
		h'
decoration (V3 dx dy dz) h (DS (Side (V2 xo zo) h')) =
	renderHouse (dx + fromIntegral xo) (dz + fromIntegral zo)
		(\_ _ -> dy + 0) -- uhhhh
		h'


antenna :: Antenna -> V3 Float -> [TRecord Surface]
antenna a o = case a of
	Stick _ i' -> let i = fromIntegral i' :: Float
		in
			[ TLine
				(TP o SWire)
				(TP (o + V3 0 (-i) 0) SWire)
			]
	Cross _ i' c' ->
		let
			i, c :: Float
			i = fromIntegral i'
			c = fromIntegral c'
		in
			[ TLine
				(TP o SWire)
				(TP (o + V3 0 (negate $ i + c) 0) SWire)
			, TLine
				(TP (o + V3 (-c) (-i) 0) SWire)
				(TP (o + V3 c (-i) 0) SWire)
			, TLine
				(TP (o + V3 0 (-i) (-c)) SWire)
				(TP (o + V3 0 (-i) c) SWire)
			]
	Net _ height' crossbar' bars' ->
		let
			height, crossbar, bars :: Float
			height = fromIntegral height'
			crossbar = fromIntegral crossbar'
			bars = fromIntegral bars'
			crossbarAt b = TLine
				(TP (o + b + V3 (-crossbar) 0 0) SWire)
				(TP (o + b + V3 crossbar 0 0) SWire)
		in
			TLine
				(TP o SWire)
				(TP (o + V3 0 (negate $ height + bars) 0) SWire)
			:
			(crossbarAt . (\y -> V3 0 (fromIntegral y) 0) <$> [negate (height' + bars') .. negate height'])

flag :: Flag -> V3 Float -> [TRecord Surface]
flag (Flag _ i' b' shape fi') o =
	[ TLine
		(TP o SWire)
		(TP (o + V3 0 (-i) 0) SWire)
	, TLine
		(TP (o + V3 0 (-i) 0) SWire)
		(TP (o + V3 0 (-i) b) SWire)
	] <> case shape of
		FlagTri ->
			[ TPoly $ Convex
				[ TP (o + V3 0 (-i) 0) SCanvas
				, TP (o + V3 0 (-i) b) SCanvas
				, TP (o + V3 0 ((-i) + fi) 0) SCanvas
				]
			]
		FlagQuad ->
			[ TPoly $ Convex
				[ TP (o + V3 0 (-i) 0) SCanvas
				, TP (o + V3 0 (-i) b) SCanvas
				, TP (o + V3 0 ((-i) + fi) b) SCanvas
				, TP (o + V3 0 ((-i) + fi) 0) SCanvas
				]
			]
	where
		i, b, fi :: Float
		i = fromIntegral i'
		b = fromIntegral b'
		fi = fromIntegral fi'

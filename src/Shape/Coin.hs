module Shape.Coin
	( coins
	, coinRender
	, materialColor

	, coinSpace

	, Coin(..)
	, Material(..)
	) where

import Linear.V2 (V2(..))
import Linear.V3
import Linear.Vector

import Data.List
import Data.Ord
import Data.Monoid
import Data.Maybe (listToMaybe)
import Control.Applicative

import Data.PossibilitySpace
import Data.Color
import Data.Turtle (TRecord(..), TPoint(..), PolyState(..), translateRecord)

import Utility.Shuffle (rotate)

import Shape

{-
procedural coins:
	outer shape (circle, polygon, oval)
	inner hole (circle, polygon)
	material (gold, silver, wood, bone, stone)
	material construction (solid, one side, inner shape + outer shape, transverse)
	size, thickness
	sides:
		markings (line, cross, shapes, figures)
			dyed, painted
		raised / embossed
	raised/lowered rim
	smooth edges / rough edges

coin sets
	denominations related to size

to start: render a filled polygon
	... with raised sides
	... with a hollow interior


(later: non-coin currency: notes (paper, silk, etc), gems (specific cuts), etc)

-}

data Material = Wood | Bone | Gold | Silver | Bronze
	deriving (Eq, Ord, Show, Read)

data Split a = Solid a | Sides a a {- | Split Material Material -}
	deriving (Eq, Ord, Show, Read)

data Internal = Filled | Hole Int Float
	deriving (Eq, Ord, Show, Read)

data Coin = Coin
	{ sides :: Int
	, size :: Float
	, thickness :: Float
	, material :: Split Material
	, internal :: Internal
	}
	deriving (Eq, Ord, Show, Read)

materialColor :: Material -> ColorRGB
materialColor m = case m of
	Wood -> RGB8 191 127 62
	Bone -> RGB8 207 207 208
	Gold -> RGB8 255 163 63
	Silver -> RGB8 239 239 239
	Bronze -> RGB8 191 143 63

coinSpace :: RenderSpace Coin Material ColorRGB
coinSpace = RenderSpace coins coinRender (const materialColor)

coins :: PossibilitySpace Coin
coins = Coin
	<$> from [3, 4, 5, 6, 7, 8, 9]
	<*> (fromIntegral <$> rangeNum (10, 25))
	<*> (fromIntegral <$> rangeNum (1, 5))
	<*> ((Solid <$> mats)
		<|> (uncurry Sides <$> twoMats)
		)
	<*> (pure Filled
		<|> (Hole
			<$> from [3, 4, 5, 6, 7, 8, 9]
			<*> ((/ 10) <$> from [1,2,3,4,5])
			)
		)
	where
		mats = from [Wood, Bone, Gold, Silver, Bronze]
		twoMats = (\opts -> case opts of
			a:b:_ -> (a, b)
			_ -> error ":(") <$> drawsOfN 2 mats

coinRender :: Coin -> [TRecord Material]
coinRender (Coin sides size thickness mat Filled) =
	[ TPoly $ Convex $ (\(V2 x y) -> TP (V3 x y (-ht)) matFront) . (size *^) <$> poly sides
	, TPoly $ Convex $ (\(V2 x y) -> TP (V3 x y ht) matBack) . (size *^) <$> reverse (poly sides)
	] <> case mat of
		Solid m -> polyWall sides size thickness m
		Sides a b -> (translateRecord (V3 0 0 (-qt)) <$> polyWall sides size ht a)
			<> (translateRecord (V3 0 0 qt) <$> polyWall sides size ht b)
	where
		ht = thickness / 2
		qt = thickness / 4
		(matFront, matBack) = case mat of
			Solid m -> (m, m)
			Sides a b -> (a, b)
coinRender (Coin sides size thickness mat (Hole holeSides holePercent)) =
	[heads, tails]
	<> case mat of
		Solid m -> polyWall sides size thickness m
			<> polyWall holeSides (size * holePercent) thickness m
		Sides a b -> (translateRecord (V3 0 0 (-qt)) <$> polyWall sides size ht a)
			<> (translateRecord (V3 0 0 qt) <$> polyWall sides size ht b)
			<> (translateRecord (V3 0 0 (-qt)) <$> polyWall holeSides (size * holePercent) ht a)
			<> (translateRecord (V3 0 0 qt) <$> polyWall holeSides (size * holePercent) ht b)
	where
		outerPoints = (size *^) <$> poly sides
		innerPoints = ((size * holePercent) *^) <$> poly holeSides
		(triPts, ixs) = rightComplement outerPoints innerPoints
		heads = TPoly $ Complex
				((\(V2 x y) -> TP (V3 x y (-ht)) matFront) <$> triPts)
				(concat ixs)
		tails = TPoly $ Complex
				((\(V2 x y) -> TP (V3 x y ht) matBack) <$> triPts)
				(concat ixs)
		ht = thickness / 2
		qt = thickness / 4
		(matFront, matBack) = case mat of
			Solid m -> (m, m)
			Sides a b -> (a, b)

poly :: Int -> [V2 Float]
poly n = fmap toPt . take n $ iterate (+ rad) 0
	where
		rad :: Float
		rad = pi * 2 / fromIntegral n
		toPt r = V2 (sin r) (cos r)

polyWall :: Int -> Float -> Float -> a -> [TRecord a]
polyWall sides size thickness a = zipWith toRect pts (rotate 1 pts)
	where
		-- toRect :: V3 Float -> V3 Float -> TRecord a
		toRect (V3 bx by bz) (V3 nx ny nz) = TPoly $ Convex
				[ TP (V3 bx by (bz + ht)) a
				, TP (V3 bx by (bz - ht)) a
				, TP (V3 nx ny (nz - ht)) a
				, TP (V3 nx ny (nz + ht)) a
				]
		ht = thickness / 2
		pts = (\(V2 x y) -> V3 x y 0) . (size *^) <$> poly sides

{- the right complement: all points in `target` that are not in `mask`

subtracts `mask` from `target` and returns a series of opengl-safe polygons (convex, with no t-intersections)

this is definitely not wholly general (it assumes `mask` is totally within `target`)
-}
rightComplement :: (RealFloat a, Floating a, Ord a) => [V2 a] -> [V2 a] -> ([V2 a], [[Int]])
rightComplement target mask =
	( tSorted <> mSorted
	, fmap resolveIndex
		. (\(p1, p2, p3) -> [p1, p2, p3])
			<$> chunk tSorted mSorted
	)
	where
			tSorted = sortBy (comparing angle) target
			mSorted = sortBy (comparing angle) mask
			resolveIndex :: Num b => (Int, b, V2 a) -> Int
			resolveIndex (ix, angle, pt) = ix
			chunk target'@(fa:as) mask'@(fb:bs) = interleaveTrisWith (\(_, b, _) -> b) tpoints mpoints
				where
					tpoints =
						zip3
							[0 .. length target' - 1]
							(angle <$> target')
							target'
						++ pure (0, angle fa + pi * 2, fa)
					mpoints =
						zip3
							((length target +) <$> [0 .. length mask' - 1])
							(angle <$> mask')
							mask'
						++ pure (length target', angle fb + pi * 2, fb)
			chunk target' _ = [] -- the mask is empty so i guess just break `target'` into tris

interleaveTrisWith :: Ord b => (a -> b) -> [a] -> [a] -> [(a,a,a)]
interleaveTrisWith with = go
	where
		go (a:as) (b:bs) = let
					(tri, (as', bs')) = lower with as bs
				in case tri of
					Just (Left t) -> (a, b, t) : go (t:as') (b:bs')
					Just (Right t) -> (a, b, t) : go (a:as') (t:bs')
					Nothing -> case listToMaybe $ as <> bs of
						Nothing -> []
						Just t -> (a, b, t) : []
		go _ _ = []

lower :: Ord b => (a -> b) -> [a] -> [a] -> (Maybe (Either a a), ([a],[a]))
lower with (a:as) (b:bs) = if with a < with b
	then (Just $ Left a, (as, b:bs))
	else (Just $ Right b, (a:as, bs))
lower with as bs = (Nothing, (as, bs))

angle :: (RealFloat a, Floating a) => V2 a -> a
angle (V2 x y) = atan2 x y

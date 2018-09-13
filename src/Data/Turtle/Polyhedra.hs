module Data.Turtle.Polyhedra
	( capsule
	, cone
	, prism
	, taggedPrism
	, taperedPrism
	, octohedron
	, splitCone
	, splitTaperedPrism

	, poly

	, at
	, rotateAroundY
	, flip
	) where

import Prelude hiding (flip)

import Linear.V2
import Linear.V3
import Linear.Quaternion
import Linear.Vector
import Linear.Metric
import Data.Turtle

import Data.List

at :: V3 Float -> V3 Float -> TRecord a -> TRecord a
at base norm = mapRecord
	$ (base +) -- translate
		. (rotate $ axisAngle axis (-theta)) -- rotate to match norm
		. (\pt -> pt + (pt * mvec)) -- stretch to fit into norm
	where
		mvec = V3 0 magnitude 0
		magnitude = distance norm 0
		(axis, theta) = axisAngleDiff (normalize norm) (V3 0 (-1) 0)

axisAngleDiff :: V3 Float -> V3 Float -> (V3 Float, Float)
axisAngleDiff a b = case (a `cross` b, acos $ a `dot` b) of
	(axis, theta) ->
		if theta /= theta -- NaN
			then (b, 0)
		else if distance axis 0 < 0.01 -- two angles parallel
			then if normalize a == V3 0 1 0 || normalize a == V3 0 (-1) 0 -- just pick an arbitrary axis that's not this one
				then (a `cross` V3 1 0 0, theta)
				else (a `cross` V3 0 1 0, theta)
		else (normalize axis, theta)



flip :: [TRecord a] -> [TRecord a]
flip = fmap $ reverseWinding . mapRecord (\pt -> pt * V3 1 (-1) 1)

rotateAroundY :: Float -> TRecord a -> TRecord a
rotateAroundY t = mapRecord (\(V3 x y z) -> V3 (x * cos t - z * sin t) y (z * cos t + x * sin t))

capsule :: Int -> Float -> V3 Int -> [TRecord ()]
capsule sides capsuleHeight scaling@(V3 x y z) = rescale <$> mconcat
	[ sidePolys -- TODO: shrink the sides slightly? i guess?
	-- place the top cap on top of the size-2 sides
	, translateRecord (V3 0 (-2) 0) . mapRecord (\pt -> pt * V3 1 capsuleHeight 1)
		<$> capSides
	, translateRecord (V3 0 (-2) 0) . mapRecord (\pt -> pt * V3 1 capsuleHeight 1)
		<$> capTop
	-- flip bottom cap
	, reverseWinding . mapRecord (\pt -> pt * V3 1 ((-1) * capsuleHeight) 1)
		<$> capSides
	, reverseWinding . mapRecord (\pt -> pt * V3 1 ((-1) * capsuleHeight ) 1)
		<$> capTop
	]
	where
		(capSides, capTop, _) = splitTaperedPrism sides 2 1 1
		(sidePolys, _, _) = splitTaperedPrism sides 2 2 2
		-- we generate the prisms as size-2 b/c we want the caps to be size 0.5 but they don't take floats, so, `2 1` is the `1 0.5`, and then we scale them by 0.5 here before applying the real scaling value. its great.
		rescale :: TRecord a -> TRecord a
		rescale = mapRecord (\p -> p * 0.5 * (fromIntegral <$> scaling))

prism :: Int -> V3 Float -> [TRecord ()]
prism = taggedPrism (const ())

taggedPrism :: (Int -> a) -> Int -> V3 Float -> [TRecord a]
taggedPrism f sides scaling = base : top : faces
	where
		faces = fmap (uncurry makeFace) . take sides $ zip [0..sides-1] $ zip ring (drop 1 ring)
		base = TPoly $ Convex
			$ zipWith (\pt i -> TP (scale pt) i)
				(take sides ring)
				(f <$> [0..sides-1])
		top = TPoly $ Convex
			$ zipWith (\pt i -> TP (scale $ pt + V3 0 1 0) i)
				(reverse (take sides ring))
				(f <$> reverse [sides.. sides*2-1])
		makeFace ix (pt1, pt2) = TPoly $ Convex
			$ zipWith (\pt i -> TP pt i)
				(scale <$> [pt2, pt1, pt1 + V3 0 1 0, pt2 + V3 0 1 0])
				(f <$> [ix+1, ix, ix+sides, ix+sides+1])
		ring = cycle $ (\(V2 x y) -> V3 x 0 y) <$> poly sides
		scale pt = pt * (scaling * V3 1 (-1) 1)

splitTaperedPrism :: Int -> Float -> Float -> Float -> ([TRecord ()], [TRecord ()], [TRecord ()])
splitTaperedPrism sides baseWidth tipWidth height = if tipWidth <= 0
	then case splitCone sides baseWidth height of
		(faces, base) -> (faces, [], base)
	else (faces, [top], [base])
	where
		faces = fmap makeFace . take sides $ zip4 baseRing (drop 1 baseRing) (drop 1 tipRing) tipRing
		base = TPoly $ Convex $ (\pt -> TP pt ()) <$> take sides baseRing
		top = TPoly $ Convex $ (\pt -> TP pt ()) <$> reverse (take sides tipRing)
		makeFace (pt1, pt2, pt3, pt4) = TPoly $ Convex $ (\pt -> TP pt ())
			<$> [pt4, pt3, pt2, pt1]
		baseRing = (^* baseWidth) <$> ring
		tipRing = (+ V3 0 h 0) . (^* tipWidth) <$> ring
		ring = cycle $ (\(V2 x y) -> V3 x 0 y) <$> poly sides
		h = height * (-1)

taperedPrism :: Int -> Float -> Float -> Float -> [TRecord ()]
taperedPrism sides baseWidth tipWidth height =
	case splitTaperedPrism sides baseWidth tipWidth height of
		(faces, top, base) -> faces <> top <> base

octohedron :: V3 Float -> [TRecord ()]
octohedron scaling = (TPoly . Convex . fmap asPt) <$>
	[ [down, front, left]
	, [down, left, back]
	, [down, back, right]
	, [down, right, front]
	, [up, left, front]
	, [up, back, left]
	, [up, right, back]
	, [up, front, right]
	]
	where
		asPt pt = TP (pt * scaling) ()
		-- i don't know if these names are actually accurate V:
		up   = V3 0 (-1) 0
		down = V3 0 1 0
		left = V3 (-1) 0 0
		right = V3 1 0 0
		front = V3 0 0 (-1)
		back = V3 0 0 1

splitCone :: Int -> Float -> Float -> ([TRecord ()], [TRecord ()])
splitCone sides baseWidth height = (faces, [base])
	where
		faces = fmap makeTri . take sides $ zip ring (drop 1 ring)
		base = TPoly $ Convex $ (\pt -> TP pt ()) <$> take sides ring
		makeTri (pt1, pt2) =
			TPoly $ Convex $ (\pt -> TP pt ()) <$> [pt2, pt1, tip]
		tip = V3 0 (-height) 0
		ring = cycle $ (baseWidth *^) . (\(V2 x y) -> V3 x 0 y) <$> poly sides

cone :: Int -> Float -> Float -> [TRecord ()]
cone sides baseWidth height = case splitCone sides baseWidth height of
	(faces, base) -> base <> faces

poly :: Int -> [V2 Float]
poly sides = fmap (\theta -> V2 (sin theta) (cos theta)) . take sides $ iterate (+ inc) 0
	where
		inc = pi * 2 / fromIntegral sides

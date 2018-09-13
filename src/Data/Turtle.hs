{-# LANGUAGE TemplateHaskell #-}

module Data.Turtle
	( TurtleAction(..)
	, Mode(..)

	, TurtleSymbol(..)
	, defaultTurtle
	, isTurtlePush
	, isTurtlePop
	, isTurtleAction

	, PolyState(..)
	, TRecord(..)
	, withUVs
	, mapRecord
	, translateRecord
	, reverseWinding
	, TPoint(..)
	, p
	, mapPoint
	, translatePoint
	, getActions
	, runActions

	-- should be somewhere else
	, deg2rad
	) where

import Control.Applicative
import Control.Arrow
import Data.List (mapAccumL)
import Data.Maybe (catMaybes, fromMaybe)

import Control.Monad.Writer

import Control.Lens

import Linear.V2
import Linear.V3
import Linear.Vector ((^*))
import Linear.Metric (Metric (..), normalize, distance)
import Linear.Matrix
import Linear.Epsilon (Epsilon)
import Linear.Quaternion

data TurtleAction a
	= DrawMode Mode
	| Color a | MutColor (a -> a)
	| Advance Float
	| Turn Float | Pitch Float | Roll Float
	| CustomRotation (Quaternion Float -> Quaternion Float)
	| Push | Pop

data Mode = Ghost | Record | Line | Polygon
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Show a => Show (TurtleAction a) where
	show a = case a of
		DrawMode m -> unwords ["DrawMode", show m]
		Color a -> unwords ["Color", show a]
		MutColor _ -> unwords ["MutColor", "<function>"]
		Advance f -> unwords ["Advance", show f]
		Turn f -> unwords ["Turn", show f]
		Pitch f -> unwords ["Pitch", show f]
		Roll f -> unwords ["Roll", show f]
		CustomRotation _ -> unwords ["CustomRotation", "<function>"]
		Push -> "Push"
		Pop -> "Pop"

data TurtleSymbol a b = TA (TurtleAction a) | A b
	deriving (Show)

isTurtlePush :: TurtleSymbol a b -> Bool
isTurtlePush t = case t of
	TA Push -> True
	_ -> False

isTurtlePop :: TurtleSymbol a b -> Bool
isTurtlePop t = case t of
	TA Pop -> True
	_ -> False

isTurtleAction :: TurtleSymbol a b -> Bool
isTurtleAction t = case t of
	TA _ -> True
	_ -> False

data Turtle a = Turtle
	{ _facing :: Quaternion Float
	, _position :: V3 Float
	, _mode :: Mode
	, _stack :: Maybe (Turtle a)
	, _color :: a
	}
	deriving (Eq, Ord, Show, Read)

data TRecord a
	= TVertex (TPoint a)
	| TLine (TPoint a) (TPoint a)
	| TPoly (PolyState (TPoint a))
	deriving (Eq, Ord, Show, Read)

instance Functor TRecord where
	fmap f (TVertex p) = TVertex $ f <$> p
	fmap f (TLine p q) = TLine (f <$> p) (f <$> q)
	fmap f (TPoly ps) = TPoly $ fmap f <$> ps

data PolyState a = Convex [a] | Complex [a] [Int]
	deriving (Eq, Ord, Show, Read)

instance Functor PolyState where
	fmap f (Convex as) = Convex $ f <$> as
	fmap f (Complex as ixs) = Complex (f <$> as) ixs

withUVs :: (TPoint a -> V2 Float -> TPoint b) -> TRecord a -> TRecord b
withUVs f (TVertex p) = TVertex $ f p (V2 0 0)
withUVs f (TLine p q) = TLine (f p (V2 0 0)) (f q (V2 0 1))
withUVs f (TPoly ps) = TPoly $ (\p -> case p of
		TP pt _ -> f p (scale lo hi $ proj pt)) <$> ps
	where
		rawPs = case ps of
			Convex as -> as
			Complex as _ -> as
		proj = case planeCoordSystem $ (\(TP p _) -> p) <$> rawPs of
			Nothing -> error $ "bad polygon (1)" <> show (const () <$> ps) -- this probably shouldn't be an error precisely
			Just (m, p) -> p
		(lo, hi) = case bb $ proj . (\(TP p _) -> p) <$> rawPs of
			Nothing -> error $ "bad polygon (2)" <> show (const () <$> ps)
			Just vs -> vs

bb :: (Num a, Ord a) => [V2 a] -> Maybe (V2 a, V2 a)
bb [] = Nothing
bb (f:rs) = Just $ foldr go (f, f) rs
	where
		go :: (Num a, Ord a) => V2 a -> (V2 a, V2 a) -> (V2 a, V2 a)
		go (V2 x y) (V2 lx ly, V2 hx hy) =
			( V2 (if x < lx then x else lx) (if y < ly then y else ly)
			, V2 (if x > hx then x else hx) (if y > hy then y else hy)
			)

scale :: Fractional a => V2 a -> V2 a -> V2 a -> V2 a
scale lo hi p = (p - lo) / range
	where
		range = hi - lo

normalFromPoly :: (Floating a, Epsilon a) => [V3 a] -> Maybe (V3 a)
normalFromPoly (o:p1:p2:_) = Just $ normalize $ (p1 - o) `cross` (p2 - o)
normalFromPoly _ = Nothing

{- assumptions this makes:
	all points in the list are on the same plane
	none of the first three points are equal to each other
-}
planeCoordSystem :: (Floating a, Epsilon a) => [V3 a] -> Maybe (M33 a, V3 a -> V2 a)
planeCoordSystem (o:p1:p2:_) = Just $ (V3 front up right, project)
	where
		frontS = (p1 - o)
		rightS = (p2 - o)
		up = normalize $ frontS `cross` rightS
		front = normalize $ rightS `cross` up
		right = up `cross` front
		project p =
			let
				xInPlane = (p - o) `dot` front
				yInPlane = (p - o) `dot` right
			in V2 xInPlane yInPlane
planeCoordSystem _ = Nothing

magnitude :: (Metric f, Floating a, Num (f a)) => f a -> a
magnitude = distance 0

mapRecord :: (V3 Float -> V3 Float) -> TRecord a -> TRecord a
mapRecord f r = case r of
	TVertex p -> TVertex $ mapPoint f p
	TLine a b -> TLine (mapPoint f a) (mapPoint f b)
	TPoly ps -> TPoly $ mapPoint f <$> ps

translateRecord :: V3 Float -> TRecord a -> TRecord a
translateRecord d r = mapRecord (d +) r

-- flip a record between front/back facing
reverseWinding :: TRecord a -> TRecord a
reverseWinding r = case r of
	TVertex p -> TVertex p
	TLine a b -> TLine b a
	TPoly sh -> TPoly $ case sh of
		Convex ps -> Convex $ reverse ps
		Complex ps is -> Complex ps $ reverse is

data TPoint a = TP (V3 Float) a
	deriving (Eq, Ord, Show, Read)

p :: TPoint a -> V3 Float
p (TP v _) = v

instance Functor TPoint where
	fmap f (TP v a) = TP v (f a)

mapPoint :: (V3 Float -> V3 Float) -> TPoint a -> TPoint a
mapPoint f (TP v a) = TP (f v) a

translatePoint :: V3 Float -> TPoint a -> TPoint a
translatePoint d p = mapPoint (d +) p

makeLenses ''Turtle

data Dictionary a b = TD
	{ resolve :: a -> b
	}

-- usingDictionary :: Dictionary a b -> LSystem m a -> LSystem m b

defaultTurtle :: a -> Turtle a
defaultTurtle a = Turtle (pitch (deg2rad 90) $ Quaternion 1 0) 0 Line Nothing a

turtleDict :: Dictionary Char (Maybe (TurtleAction a))
turtleDict = TD
	{ resolve = \a -> case a of
		'v' -> Just $ DrawMode Line
		'^' -> Just $ DrawMode Ghost
		'F' -> Just $ Advance 1
		'f' -> Just $ Advance 1
		'-' -> Just $ Turn (-60)
		'+' -> Just $ Turn 60
		'T' -> Just $ Advance 1
		'[' -> Just Push
		']' -> Just Pop
		'L' -> Nothing
		'X' -> Nothing
		'Y' -> Nothing
		_ -> error "idk?"
	}

heading :: Float -> Quaternion Float -> Quaternion Float
heading θ q = q * axisAngle (V3 0 1 0) θ

pitch :: Float -> Quaternion Float -> Quaternion Float
pitch θ q = q * axisAngle (V3 1 0 0) θ

roll :: Float -> Quaternion Float -> Quaternion Float
roll θ q = q * axisAngle (V3 0 0 1) θ

getActions :: [TurtleSymbol a b] -> [TurtleAction a]
getActions [] = []
getActions (TA a:rs) = a : getActions rs
getActions (A _:rs) = getActions rs

-- since TPoints are a set of linear points, there's no way to see a movement corresponding to a position pop, which means the `last` value isn't correctly set
asRecord :: [(Mode, TPoint a)] -> [TRecord a]
asRecord = go [] Nothing
	where
		go acc _ [] = fromMaybe [] $ pure <$> resolveAcc acc
		go acc last (t:rs) = case t of
			(Ghost, _) -> go acc last rs
			(Record, pt) -> TVertex pt : resolvedAcc (go [] (Just pt) rs)
			(Line, pt) -> case last of
				Just prev -> TLine prev pt : resolvedAcc (go [] (Just pt) rs)
				Nothing -> resolvedAcc $ go [] (Just pt) rs
			(Polygon, pt) -> go (pt:acc) (Just pt) rs
			where
				resolvedAcc = case resolveAcc acc of
					Nothing -> id
					Just x -> (x :)

		resolveAcc :: [TPoint a] -> Maybe (TRecord a)
		resolveAcc [] = Nothing
		resolveAcc ps = Just $ TPoly $ Convex ps

runActions :: Turtle a -> [TurtleAction a] -> [TRecord a]
runActions z acts = asRecord $ (z ^. mode, TP (z ^. position) (z ^. color)) : (catMaybes $ snd $ mapAccumL turtleAction z acts)

turtleActionW :: Turtle a -> TurtleAction a -> Writer [Maybe (Mode, TPoint a)] (Turtle a)
turtleActionW t act = writer . second pure $ turtleAction t act

turtleAction :: Turtle a -> TurtleAction a -> (Turtle a, Maybe (Mode, TPoint a))
turtleAction t act = case act of
	DrawMode m -> (mode .~ m $ t, case m of
			Ghost -> Nothing
			_ -> Just (m, TP (t ^. position) (t ^. color))
		)
	Color a -> (color .~ a $ t, Nothing)
	MutColor f -> (color %~ f $ t, Nothing)
	Advance x ->
		let step = move (t ^. facing) x
		in
			( position %~ (+ step) $ t
			, case t ^. mode of
				Ghost -> Nothing
				m -> Just (m, TP ((t ^. position) + step) (t ^. color))
			)
{-
	Backward x ->
		let step = move (t ^. facing) (-x)
		in (position %~ (+ step) $ t, if t ^. pen
			then Just $ Line (t ^. position, (t ^. position) + step)
			else Nothing)
-}
--	TurnLeft x -> (facing %~ heading (-x) $ t, Nothing)
	Turn x -> (facing %~ heading x $ t, Nothing)
--	PitchUp x -> (facing %~ pitch (-x) $ t, Nothing)
	Pitch x -> (facing %~ pitch x $ t, Nothing)
--	RollLeft x -> (facing %~ roll (-x) $ t, Nothing)
	Roll x -> (facing %~ roll x $ t, Nothing)
	CustomRotation f -> (facing %~ f $ t, Nothing)
	Push -> (t { _stack = Just t }, Nothing)
	Pop -> case t ^. stack of
		Nothing -> error "stack underflow"
		Just t' -> (t', Just (Record, TP (t' ^. position) (t' ^. color))) -- record the pop in the move list, so that the right value gets used for lines right after the pop

move :: Quaternion Float -> Float -> V3 Float
move q d = rotate q (V3 0 0 d)

deg2rad :: Float -> Float
deg2rad deg = deg / 180 * pi

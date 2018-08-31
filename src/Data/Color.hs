module Data.Color
	( ColorHSL(..)
	, ColorRGB(..)
	, ColorRGBA(..)
	, hsl2rgb
	, withAlpha
	, asHex

	, BlendableLight(..)
	) where

import Numeric (showHex)
import Data.Monoid
import Data.Fixed

import Linear.V4

data ColorHSL a = HSL
	{ hue :: a -- [0..360)
	, saturation :: a -- [0..1]
	, lightness :: a -- [0..1]
	}
	deriving (Eq, Ord, Show, Read)

instance Functor ColorHSL where
	fmap f (HSL h s l) = HSL (f h) (f s) (f l)

instance Semigroup a => Semigroup (ColorHSL a) where
	HSL h s l <> HSL h' s' l' = HSL (h <> h') (s <> s') (l <> l')

instance Monoid a => Monoid (ColorHSL a) where
	mempty = HSL mempty mempty mempty

instance (Num a, Ord a) => Num (ColorHSL a) where
	(HSL h s l) + (HSL h' s' l') = HSL (h + h') (s + s') (l + l')
	(HSL h s l) - (HSL h' s' l') = HSL (h - h') (s - s') (l - l')
	(HSL h s l) * (HSL h' s' l') = HSL (h * h') (s * s') (l * l')
	negate (HSL h s l) = HSL (-h) (-s) (-l)
	abs (HSL h s l) = HSL (abs h) (abs s) (abs l)
	signum (HSL h s l) = HSL (iff (h > 0) 1 (-1)) (iff (s > 0) 1 (-1)) (iff (l > 0) 1 (-1))
		where
			iff b t f = if b then t else f
	fromInteger i = HSL (fromInteger i) (fromInteger i) (fromInteger i)

data ColorRGB = RGB8 !Int !Int !Int
	deriving (Eq, Ord, Show, Read)

instance Num ColorRGB where
	(RGB8 r g b) + (RGB8 r' g' b') = RGB8 (r + r') (g + g') (b + b')
	(RGB8 r g b) - (RGB8 r' g' b') = RGB8 (r - r') (g - g') (b - b')
	(RGB8 r g b) * (RGB8 r' g' b') = RGB8 (r * r') (g * g') (b * b')
	negate (RGB8 r g b) = RGB8 (-r) (-g) (-b)
	abs (RGB8 r g b) = RGB8 (abs r) (abs g) (abs b)
	signum (RGB8 r g b) = RGB8 (iff (r > 0) 1 (-1)) (iff (g > 0) 1 (-1)) (iff (b > 0) 1 (-1))
		where
			iff b t f = if b then t else f
	fromInteger i = RGB8 (fromIntegral i) (fromIntegral i) (fromIntegral i)

data ColorRGBA = RGBA8 !Int !Int !Int !Int
	deriving (Eq, Ord, Show, Read)

-- this is a kludge and isn't necessarily reasonable or correct
instance Num ColorRGBA where
	(RGBA8 r g b a) + (RGBA8 r' g' b' a') = RGBA8 (r + r') (g + g') (b + b') (intMult a a')
	(RGBA8 r g b a) - (RGBA8 r' g' b' a') = RGBA8 (r - r') (g - g') (b - b') (intMult a (255 - a'))
	(RGBA8 r g b a) * (RGBA8 r' g' b' a') = RGBA8 (r * r') (g * g') (b * b') (intMult a a')
	negate (RGBA8 r g b a) = RGBA8 (-r) (-g) (-b) a
	abs (RGBA8 r g b a) = RGBA8 (abs r) (abs g) (abs b) (abs a)
	signum (RGBA8 r g b a) = RGBA8
		(iff (r > 0) 1 (-1))
		(iff (g > 0) 1 (-1))
		(iff (b > 0) 1 (-1))
		(iff (a > 0) 1 (-1))
		where
			iff b t f = if b then t else f
	fromInteger i = RGBA8 (fromIntegral i) (fromIntegral i) (fromIntegral i) 255

intMult :: Int -> Int -> Int
intMult a b = (a * b) `div` 255

withAlpha :: Int -> ColorRGB -> ColorRGBA
withAlpha a (RGB8 r g b) = RGBA8 r g b a



hsl2rgb :: ColorHSL Float -> ColorRGB
hsl2rgb color = RGB8 mi mi mi + (case h' of
	h'
		| h' `between` (0, 1) -> RGB8 ci xi  0
		| h' `between` (1, 2) -> RGB8 xi ci  0
		| h' `between` (2, 3) -> RGB8  0 ci xi
		| h' `between` (3, 4) -> RGB8  0 xi ci
		| h' `between` (4, 5) -> RGB8 xi  0 ci
		| h' `between` (5, 6) -> RGB8 ci  0 xi
		| otherwise -> RGB8 0 0 0)
	where
		c = (1 - abs (2 * lightness color - 1)) * saturation color
		h' = (hue color / 60) `mod'` 6
		x = c * (1 - abs ((h' `mod'` 2) - 1))
		m = lightness color - (0.5 * c)
		mi, ci, xi :: Int
		mi = floor $ m * 255
		ci = floor $ c * 255
		xi = floor $ x * 255

asHex :: ColorRGB -> String
asHex (RGB8 r g b) = duplet 2 r ++ duplet 2 g ++ duplet 2 b
	where
		duplet len x = (\o -> replicate (max 0 $ len - length o) '0' ++ o) . showHex (clamp 0 255 x) $ ""
		clamp lo hi x = min hi $ max lo x

between :: Ord a => a -> (a, a) -> Bool
between x (lo, hi) = lo <= x && x < hi

class Num a => BlendableLight a where
	scale :: a -> Float -> a
	merge :: a -> a -> a
	asColor :: a -> V4 Float

-- this is a kludge and isn't necessarily reasonable
instance BlendableLight ColorRGB where
	scale (RGB8 r g b) s = RGB8
		(floor $ fromIntegral r * s)
		(floor $ fromIntegral g * s)
		(floor $ fromIntegral b * s)
	merge (RGB8 r g b) (RGB8 r' g' b') = RGB8
		(r * r' `div` 255)
		(g * g' `div` 255)
		(b * b' `div` 255)
	asColor (RGB8 r g b) = (/ 255) . fromIntegral <$> V4 r g b 255

instance BlendableLight ColorRGBA where
	scale (RGBA8 r g b a) s = RGBA8
		(floor $ fromIntegral r * s)
		(floor $ fromIntegral g * s)
		(floor $ fromIntegral b * s)
		a
	merge (RGBA8 r g b a) (RGBA8 r' g' b' a') = RGBA8
		(r * r' `div` 255)
		(g * g' `div` 255)
		(b * b' `div` 255)
		(min a a')
	asColor (RGBA8 r g b a) = (/ 255) . fromIntegral <$> V4 r g b a

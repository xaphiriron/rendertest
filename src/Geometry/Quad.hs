module Geometry.Quad where

import Data.Monoid
import Linear.V2

data Quad a = Quad (V2 a) (V2 a)
	deriving (Eq, Ord, Show, Read)

instance Functor Quad where
	fmap f (Quad xy wl) = Quad (f <$> xy) (f <$> wl)

transposeQuad :: Quad a -> Quad a
transposeQuad (Quad (V2 x y) (V2 w l)) =
	Quad (V2 y x) (V2 l w)

quadIntersection :: (Num a, Ord a) => Quad a -> Quad a -> Maybe (Quad a)
quadIntersection (Quad (V2 xo1 yo1) (V2 xl1 yl1)) (Quad (V2 xo2 yo2) (V2 xl2 yl2))
	= case (,)
				<$> intersection xo1 xl1 xo2 xl2
				<*> intersection yo1 yl1 yo2 yl2 of
			Nothing -> Nothing
			Just ((xo', xl'), (yo', yl')) -> Just $ Quad (V2 xo' yo') (V2 xl' yl')

quadLeftComplement :: (Num a, Ord a) => Quad a -> Quad a -> [Quad a]
quadLeftComplement (Quad (V2 xo1 yo1) (V2 xl1 yl1)) (Quad (V2 xo2 yo2) (V2 xl2 yl2))
	= foo (leftComplement xo1 xl1 xo2 xl2) (leftComplement yo1 yl1 yo2 yl2)
	where
		foo xs ys = (toQuad <$> xs <*> ys) <> case (xs, ys) of
			([], []) -> []
			([], ys) -> (\(yo, yl) -> Quad (V2 xo1 yo) (V2 xl1 yl)) <$> ys
			-- L shape, with one diagonal and two manually-placed quads
			([(xo, xl)], [(yo, yl)]) ->
				[ Quad (V2 (if xo > xo1 then xo1 else xo + xl) yo) (V2 (xl1 - xl) yl)
				, Quad (V2 xo (if yo > yo1 then yo1 else yo + yl)) (V2 xl (yl1 - yl))
				]
			-- C shape, with one y offset and two x offsets
			([(xo, xl)], [(yo_a, yl_a), (yo_b, yl_b)]) ->
				-- y middle (y offset from both y diagonal)
				[ Quad (V2 xo (yo_a + yl_a)) (V2 xl (yl1 - (yl_a + yl_b)))
				-- x offset from either y diagonal
				, Quad (V2 (if xo > xo1 then xo1 else xo + xl) yo_a) (V2 (xl1 - xl) yl_a)
				, Quad (V2 (if xo > xo1 then xo1 else xo + xl) yo_b) (V2 (xl1 - xl) yl_b)
				]
			-- O shape, with two x and y offsets
			([(xo_a, xl_a), (xo_b, xl_b)], [(yo_a, yl_a), (yo_b, yl_b)]) ->
				-- the four central sectors; middle x:
				[ Quad (V2 (xo_a + xl_a) yo_a) (V2 (xl1 - (xl_a + xl_b)) yl_a)
				, Quad (V2 (xo_a + xl_a) yo_b) (V2 (xl1 - (xl_a + xl_b)) yl_b)
				-- middle y:
				, Quad (V2 xo_a (yo_a + yl_a)) (V2 xl_a (yl1 - (yl_a + yl_b)))
				, Quad (V2 xo_b (yo_a + yl_a)) (V2 xl_b (yl1 - (yl_a + yl_b)))
				]
			(xs, ys) -> transposeQuad <$> foo ys xs
		toQuad (xo, xl) (yo, yl) = Quad (V2 xo yo) (V2 xl yl)

intersection :: (Num a, Ord a) => a -> a -> a -> a -> Maybe (a, a)
intersection o1 l1 o2 l2 | o1 > o2 = intersection o2 l2 o1 l1
	| otherwise = if o1 + l1 < o2
		then Nothing
		else Just (o2, min (o2 + l2) (o1 + l1))

-- returns a list of pairs in the form of (starting coordinate, length)
leftComplement :: (Num a, Ord a) => a -> a -> a -> a -> [(a, a)]
leftComplement o1 l1 o2 l2 =
	if o1 > o2
		then if o1 > o2 + l2
			then (o1, l1) : []
			else []
		else (o1, min l1 (o2 - o1)) : if o1 + l1 > o2 + l2
			then let o = o2 + l2 in (o, (o1 + l1) - o) : []
			else []

rightComplement :: (Num a, Ord a) => a -> a -> a -> a -> [(a, a)]
rightComplement o1 l1 o2 l2 = leftComplement o2 l2 o1 l1

module Shape.Hex
	( Hex(..)
	, adjacent
	, sharedAdjacent
	, render
	, renderCorners
	, rotate
	, rotateAround
	, magnitude
	, distance
	, ring
	, fx

	, Shaped(..)

	, Shape(..)
	, SubShape(..)
	, rotateShapeAt
	, rotateShape
	, shapeOverlap
	, shapeFullyInside
	, shapeAdjacent
	, shapeConnections
	, containedInShape
	, shapeSize
	, adjacentToShape
	, constructPhantomShape

	, outlineShape

	, moveShapeTo
	, addToShape

	, unlineate
	, rki2hex
	, hex2rki
	) where

import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set, intersection, (\\), member)

data Hex = Hex Int Int
	deriving (Eq, Ord, Show, Read)

instance Num Hex where
	Hex ax ay + Hex bx by = Hex (ax+bx) (ay+by)
	Hex ax ay - Hex bx by = Hex (ax-bx) (ay-by)
	Hex ax ay * Hex bx by = Hex (ax*bx) (ay*by)
	negate (Hex x y) = Hex (x * (-1)) (y * (-1))
	abs (Hex x y) = Hex (abs x) (abs y)
	signum (Hex x y) = Hex (signum x) (signum y)
	fromInteger i = Hex (fromIntegral i) (fromIntegral i)

data Shape
	= HSize Hex Int -- origin / radius
	| Sub Hex Int SubShape -- origin / rotation / shape
	| Abstract Hex [Hex]
	deriving (Eq, Ord, Show, Read)

data SubShape = Line Int | Diamond Int Int | Triangle Int | Trapezoid Int Int
-- | IrregularHex Int Int Int
	deriving (Eq, Ord, Show, Read)

class Shaped a where
	(^*) :: Int -> a -> a
	s ^* a = scalarOp (*s) a
	(*^) :: a -> Int -> a
	a *^ s = scalarOp (*s) a
	scalarOp :: (Int -> Int) -> a -> a

adjacent :: Hex -> [Hex]
adjacent (Hex x y) =
	[ Hex  x    (y+1)
	, Hex (x+1) (y+1)
	, Hex (x+1)  y
	, Hex  x    (y-1)
	, Hex (x-1) (y-1)
	, Hex (x-1)  y
	]

instance Shaped Hex where
	scalarOp f (Hex x y) = Hex (f x) (f y)

instance Shaped Shape where
	scalarOp f (HSize c r) = HSize (scalarOp f c) r
	scalarOp f (Sub c rot sub) = Sub (scalarOp f c) rot sub
	scalarOp f (Abstract c hs) = Abstract (scalarOp f c) hs

invertShape :: Shape -> Shape
invertShape shape = case shape of
	Abstract c hs -> Abstract c $ (\v -> v *^ (-1)) <$> hs
	Sub o rot sub -> Sub o ((rot + 3) `mod` 6) sub
	HSize _ _ -> shape

-- given two (complex, abstract) shapes, this will return a new shape which has a border (i.e., adjacentToShape output) that corresponds to valid locations the center of the first argument shape can be placed to make it adjacent to the second argument shape. the actual position value of the first shape is ignored.
-- note that you might want to call this multiple times with the first argument shape rotated
constructPhantomShape :: Shape -> Shape -> Shape
constructPhantomShape s match = case Set.toList allHexes of
	[] -> error "constructPhantomShape: bad shapes"
	o:rs -> Abstract o $ (\h -> h - o) <$> (o:rs)
	where
		iShape = invertShape s
		allHexes :: Set Hex
		allHexes = Set.unions $ (\h -> Set.fromList $ containedInShape $ moveShapeTo h iShape)
			<$> containedInShape match

outlineShape :: Float -> Shape -> [(Float, Float)]
outlineShape _ (Abstract _ _) = error "can't make outline for arbitrary shape"
outlineShape scale (Sub o rot shape) = case shape of
	Line l -> let a = rotate rot $ Hex 0 l
		in ((add (render o) . (smult scale)) <$> nth (rot + 2) 3)
			++ ((add (render (o + a)) . (smult scale)) <$> nth (rot + 5) 3)
	Triangle l ->
		let
			a = o + rotate rot (Hex 0 l)
			b = o + rotate rot (Hex l l)
		in (edgePt o <$> nth (rot + 3) 2)
			++ (edgePt a <$> nth (rot + 5) 2)
			++ (edgePt b <$> nth (rot + 1) 2)
	Diamond x y ->
		let
			a = o + rotate rot (Hex 0 y)
			b = o + rotate rot (Hex x y)
			c = o + rotate rot (Hex x 0)
		in
			(edgePt o <$> nth (rot + 4) 1)
				++ (edgePt a <$> nth (rot + 5) 2)
				++ (edgePt b <$> nth (rot + 1) 1)
				++ (edgePt c <$> nth (rot + 2) 2)
	Trapezoid n m ->
		let
			a = o + rotate rot (Hex 0 n)
			b = o + rotate rot (Hex m n)
			c = o + rotate rot (Hex m m)
		in
			(edgePt o <$> nth (rot + 3) 2)
				++ (edgePt a <$> nth (rot + 5) 2)
				++ (edgePt b <$> nth (rot + 1) 1)
				++ (edgePt c <$> nth (rot + 2) 1)
	where
		edgePt :: Hex -> (Float, Float) -> (Float, Float)
		edgePt p xy = render p `add` smult scale xy
--	_ -> error "not implemented yet for subshapes"
outlineShape scale (HSize c r) = edges
	where
		edges :: [(Float, Float)]
		edges = ((\(x, y) -> ((x - cx) * ra + cx, (y - cy) * ra + cy) ) . render)
			<$> adjacent c
		ra = fromIntegral r + scale
		(cx, cy) = render c
		{-
		edges :: [(Float, Float)]
		edges = (\(k, hs) ->
				(\(i, h) -> let
					ra = 0.66
					(cx, cy) = render h
					needed = if i == 0 then 4 else 3
					k' = if i == 0 then k else (k+1)
				in
					(\(x, y) -> ((x - cx) * ra + cx, (y - cy) * ra + cy))
						<$> (take needed . drop k' . cycle . renderCorners $ h))
				=<< zip [0..] hs)
{-
				(\h -> let
					ra = 0.66
					(cx, cy) = render h
				in
					(\(x, y) -> ((x - cx) * ra + cx, (y - cy) * ra + cy))
						<$> (take 3 . drop i . cycle . renderCorners $ h))
				=<< hs)-}
			=<< (zip [0..] $ groupInto r $ ring c r)
			-}
{-
groupInto :: Int -> [a] -> [[a]]
groupInto _ [] = []
groupInto i vs = take i vs : groupInto i (drop i vs)
-}

nth :: Int -> Int -> [(Float, Float)]
nth i n = take n . drop i . cycle $ fmap render $ adjacent 0

{-
-- this is also probably quadratic time
sharedAdjacent :: Hex -> Hex -> [Hex]
sharedAdjacent a b = filter (\h -> h `elem` adjacent b) $ adjacent a
-}
sharedAdjacent :: [Hex] -> Set Hex
sharedAdjacent [] = Set.empty
sharedAdjacent [a] = Set.fromList $ adjacent a
sharedAdjacent (a:rs) = Set.fromList (adjacent a) `intersection` sharedAdjacent rs

render :: Hex -> (Float, Float)
render (Hex x y) = (rx, ry)
	where
		rx = fromIntegral x * 26
		ry = fromIntegral x * 15 + fromIntegral y * (-30)

renderCorners :: Hex -> [(Float, Float)]
renderCorners h = add (render h) <$>
	[ ((-17.33),   0 )
	, (( -8.66),(-15))
	, (   8.66 ,(-15))
	, (  17.33 ,   0 )
	, (   8.66 ,  15 )
	, (( -8.66),  15 )
	]

add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (x, y) (z, w) = (x+z, y+w)

smult :: Num a => a -> (a, a) -> (a, a)
smult v (x, y) = (x * v, y * v)

rotateAround :: Hex -> Int -> Hex -> Hex
rotateAround pivot turns p = (+ pivot) . rotate turns . subtract pivot $ p

rotate :: Int -> Hex -> Hex
rotate turns pt = head . drop (turns `mod` 6) . iterate cw $ pt
	where
		cw (Hex x y) = Hex y (y - x)
--		ccw (Hex x y) = Hex (x - y) x

magnitude :: Hex -> Int
magnitude = distance 0

distance :: Hex -> Hex -> Int
distance a b = case (neg x, neg y) of
	(False, False) -> max x y
	( True,  True) -> max (abs x) (abs y)
	( True, False) -> abs x + y
	(False,  True) -> x + abs y
	where
		neg x = x < 0
		(Hex x y) = b - a

rotateShapeAt :: Hex -> Int -> Shape -> Shape
rotateShapeAt pivot turns shape = case shape of
	Abstract o pts -> Abstract (rotateAround pivot turns o) $ rotateAround 0 turns <$> pts
	Sub o rot sub -> Sub (rotateAround pivot turns o) ((rot + turns) `mod` 6) sub
	HSize center size -> HSize (rotateAround pivot turns center) size

rotateShape :: Shape -> Int -> Shape
rotateShape h 0 = h
rotateShape shape turns = case shape of
	Abstract c hs -> Abstract c $ rotateAround 0 turns <$> hs
	Sub o rot sub -> Sub o ((rot + turns) `mod` 6) sub
	HSize _ _ -> shape

-- this is i think extremely inefficient (quadratic time) so don't use this for really large data sets. if you do need to use it for large datasets, sort both lists and then do an interleave filter; that would probably be more efficient. maybe?? (i mean you'd think but i have no clue what the space/time for anything in haskell is) (or just convert to set and use its intersection function)
shapeOverlap :: Shape -> Shape -> [Hex]
shapeOverlap a b = filter (\h -> h `elem` containedInShape b) (containedInShape a)

shapeFullyInside :: Shape -> Shape -> Bool
{-
shapeFullyInside (HSize c r) (HSize c' r') = distance c c' <= r - r'
-}
shapeFullyInside container s = case container of
	HSize c r
		| distance c (base s) > r -> False
{- -- these aren't actually right
		| otherwise -> case s of
			HSize o r'
				| distance c o < r + r' -> True
				| distance c o >= r + r' -> False
			Sub o _ (Line i)
				| distance c o < r + i -> True
			Sub o _ (Diamond i j)
				| distance c o < r + i + j -> True
			Sub o _ (Triangle i)
				| distance c o < r + i -> True
			Sub o _ (Trapezoid i j)
				| distance c o < r + i + j -> True
			_ -> generalComparison
-}
	_ -> generalComparison
	where
		generalComparison = sHexes `Set.isSubsetOf` cHexes
			where
				cHexes = Set.fromList (containedInShape container)
				sHexes = Set.fromList (containedInShape s)

shapeAdjacent :: Shape -> Shape -> Set Hex
shapeAdjacent a b = Set.fromList (containedInShape a) `intersection` adjacentToShape b

shapeConnections :: Shape -> Shape -> Set (Hex, Hex)
shapeConnections a b =
	Set.fromList $
	(\ap -> (,)
		<$> pure ap
		<*> (filter (\x -> x `member` bPts) $ adjacent ap)
	)
		=<< aPts
	where
		aPts :: [Hex]
		aPts = Set.toList $ Set.fromList (containedInShape a) `intersection` adjacentToShape b -- points in `a` that are adjacent to at least one tile in `b`
		bPts :: Set Hex
		bPts = Set.fromList (containedInShape b)

containedInShape :: Shape -> [Hex]
containedInShape (Abstract o x) = (o +) <$> x
containedInShape (HSize center radius) = ring center =<< [0..radius]
containedInShape (Sub o rot subShape) = (o +) . rotateAround 0 rot <$> case subShape of
		Line i -> line i
		Diamond i j -> diamond i j
		Triangle i -> triangle i
		Trapezoid i j -> trapezoid i j
	where
		line :: Int -> [Hex]
		line l = Hex 0 <$> [0..l]

		diamond :: Int -> Int -> [Hex]
		diamond n m = Hex <$> [0..n] <*> [0..m]

		triangle :: Int -> [Hex]
		triangle l = [Hex j i | i <- [0..l], j <- [0..i]]

		trapezoid :: Int -> Int -> [Hex]
		trapezoid n m = (\mi -> Hex mi <$> [mi..n]) =<< [0..m]

shapeSize :: Shape -> Int
shapeSize = length . containedInShape

ring :: Hex -> Int -> [Hex]
ring c 0 = [c]
ring c n = ((c +) . rki2hex) <$> [(r, k, i) | r <- [n], k <- [0..5], i <- [0..r-1]]

adjacentToShape :: Shape -> Set Hex
adjacentToShape (HSize c r) = Set.fromList $ ring c (r+1)
-- this is really not gonna be efficient, i think
adjacentToShape s = (Set.fromList $ adjacent =<< containedInShape s) \\ (Set.fromList $ containedInShape s)

base :: Shape -> Hex
base (HSize c _) = c
base (Abstract c _) = c
base (Sub c _ _) = c

moveShapeTo :: Hex -> Shape -> Shape
moveShapeTo c (HSize _ r) = HSize c r
moveShapeTo c (Abstract _ hs) = Abstract c hs
moveShapeTo c (Sub _ rot sub) = Sub c rot sub

addToShape :: Hex -> Shape -> Shape
addToShape a (HSize c r) = HSize (c + a) r
addToShape a (Abstract c hs) = Abstract (c + a) hs
addToShape a (Sub c rot sub) = Sub (c + a) rot sub

-- i'm not 100% sure this works right in all cases yet
hex2rki :: Hex -> (Int, Int, Int)
hex2rki (Hex 0 0) = (0, 0, 0)
hex2rki (Hex x y) = (r, k, i)
	where
		xyMag (x', y') = if signum x' /= signum y'
			then abs $ negate x' + y'
			else max (abs x') (abs y')
		r = xyMag (x, y)
		(k, i) = case kiResolve `mapMaybe` [0..5] of
			vs:_ -> vs
			[] -> error "hex2rki: can't convert hex coord(?!)"
		kiResolve :: Int -> Maybe (Int, Int)
		kiResolve o = case (\(Hex kx ky) -> Hex (x - kx * r) (y - ky * r)) $ hexLines o of
			Hex 0 0 -> Just (o, 0)
			Hex ix iy -> let
					i = xyMag (ix, iy)
				in
					if i >= r
						then Nothing
						else Just (o, i)

rki2hex :: (Int, Int, Int) -> Hex
rki2hex (r, k, i) = ka + ia
	where
		ka = hexLines k *^ r
		ia = hexLines (k + 2) *^ i

hexLines :: Int -> Hex
hexLines i = adjacent 0 !! (i `mod` 6)

unlineate :: Int -> (Int, Int, Int)
unlineate 0 = (0, 0, 0)
unlineate n = (r, k, i)
	where
		r = head $ dropWhile (\rad -> fx rad <= n) $ [0..]
		pastRingStart = n - fx (r - 1) -- r - fx (r-1) is the number of tiles in the outer ring (number of tiles total - number of tiles in all prior rings). n - fx (r-1) is the number of tiles n is past the edge of the last ring
		k = pastRingStart `div` r
		i = pastRingStart `mod` r

-- number of hexes in a hex platter with a radius of i (1, 7, 19, 37, ...)
fx :: Int -> Int
fx n = 3 * n * (n + 1) + 1

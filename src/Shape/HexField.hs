{-# LANGUAGE DeriveFunctor #-}

module Shape.HexField
	( shapes
	, rainbowColor
	, flatColor
	, heightShapes

	, toFlatSlope

	, SlopeHex(..)
	, Slope(..)
	) where

import Control.Arrow (first, second)
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Function
import Data.Fixed
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Map (Map)

import Linear.V2
import Linear.V3
import Linear.Vector hiding (basis)
import Linear.Metric (normalize)

import Data.Turtle hiding (Line, TurtleSymbol(..))
import Data.Color
import Data.PossibilitySpace

import Shape
import Shape.Hex hiding ((^*))

import Utility.Tuple
import Utility.Rand
import Utility.Shuffle

import Debug.Trace
{-
fieldSpace :: RenderSpace Shape Float
fieldSpace = RenderSpace shapes flatShape rainbowColor
-}
-- generatedFieldGraph :: ... -> RenderSpace ... ...

shapes :: PossibilitySpace Shape
shapes =
	(HSize 0 <$> rangeNum (0, 6))
	<|> (Sub 0
		<$> rangeNum (0, 5)
		<*> ((Line <$> rangeNum (1, 6))
			<|> (Diamond <$> rangeNum (1, 6) <*> rangeNum (1, 6))
			<|> (Triangle <$> rangeNum (1, 6))
{-
			<|> (uncurry Trapezoid <$> ((\v -> case v of
				lo : hi : _ -> (hi, lo)
				_ -> error "bad draw") <$> uniqueDrawsOfN 2 (rangeNum (1, 6)))) -}
			)
		)

flatColor :: a -> ColorRGB
flatColor _ = RGB8 0xf8 0xfa 0xf4

rainbowColor :: Float -> ColorRGB
rainbowColor i = hsl2rgb $ HSL (i * 15) 1 0.5
{-
flatShape :: Shape -> [TRecord Float]
flatShape s = let hexes = containedInShape s
	in generateHexMesh . sliceHexes $ hexes

flatShapes :: [Shape] -> [TRecord Float]
flatShapes ss = let hexes = containedInShape =<< ss
	in generateHexMesh . sliceHexes $ hexes
-}
heightShapes :: (RandomGen g, Show d)
	=> ((SlopeHex, d) -> Rand g [TRecord d]) -- decor
	-> ((Int, Shape, a) -> Rand g [(Hex, (Int, b))]) -- placement
	-> (Map Hex (Int, b) -> Hex -> (Int, b) -> (Int, c)) -- process
	-> (Map Hex (Int, c) -> Hex -> (Int, c) -> Rand g (SlopeHex, d)) -- smooth
	-> (d -> Maybe (Int, d)) -- hasWater
	-> [(Int, Shape, a)] -- ss
	-> Rand g [TRecord d]
heightShapes decor placement process smooth hasWater ss = do
		rawHexes <- fmap concat $ sequence $ placement <$> ss -- b
		let hexHeights = fmap fillGaps . sliceHexes $ rawHexes -- b

		-- it would be nice if these two weren't hardcoded but w/e it works i guess
		-- run smoothing pass
		let processedHexes = mapWithContext process hexHeights
		-- run sloping pass
		slopedHexes <- mapWithContextM smooth processedHexes

		generateHexMesh (\(sl, a) -> case hasWater a of
			Nothing -> [(CL 0, (sl, a))]
			Just (h, w) -> [(CL 0, (sl, a)), (CL 1, (toFlatSlope h, w))]
			) decor $ slopedHexes
	where
		-- hexHeights = fmap fillGaps . sliceHexes $ placement =<< ss
		-- placement = fromMaybe (pure . placeShapeHexes) customPlacement
		-- smooth = fromMaybe (\_ _ -> pure . first toFlatSlope) smoothProcess

placeShapeHexes :: (Int, Shape, a) -> [(Hex, (Int, a))]
placeShapeHexes (height, shape, color) =
	(,) <$> containedInShape shape <*> pure (height, color)

generateHexMesh :: (RandomGen g, Show a, Show b) =>
	(a -> [(ConnectiveLayer, (SlopeHex, b))]) -> -- extractFunc
	((SlopeHex, b) -> Rand g [TRecord b]) -> -- decoration
	[HexStrip a] -> -- strips
	Rand g [TRecord b]
generateHexMesh extractFunc decoration strips = let
			(tris, last, rands, pending) = foldl' calcSpandrels ([], 0, pure [], [])
				$ fmap extractFunc <$> strips
		in do
			decor <- rands
			return $ decor <> tris <> (spandrelTri (last + 1) =<< pending)
	where
		calcSpandrels (tris, _, rands, backs) strip@(HexStrip x _) =
				( tris <> getTiles strip <> (spandrelTri x =<< completeBacks)
				, x
				, (<>) <$> rands <*> decorations
				, fronts
				)
			where
				getTiles strip = concat . contents
					$ mapWithHex (\h layers -> uncurry (tile h) =<< (snd <$> layers)) strip
				decorations = decorateStrip decoration $ fmap snd <$> strip
				(completeBacks, fronts) = patches backs strip



data HexStrip a = HexStrip Int [(Int, a)]
	deriving (Eq, Ord, Show, Read)

instance Functor HexStrip where
	fmap f (HexStrip x ys) = HexStrip x $ (\(y, a) -> (y, f a)) <$> ys

contents :: HexStrip a -> [a]
contents (HexStrip _ ys) = snd <$> ys

mapWithHex :: (Hex -> a -> b) -> HexStrip a -> HexStrip b
mapWithHex f (HexStrip x ys) = HexStrip x $ (\(y, a) -> (y, f (Hex x y) a)) <$> ys

{- map across a set of hex strips, with the context of the six adjacent tiles plus the hex coord of each hex.
-}
mapWithContext :: (Map Hex a -> Hex -> a -> b) -> [HexStrip a] -> [HexStrip b]
mapWithContext f = go mempty (HexStrip 0 [])
	where
		go c last@(HexStrip lastX _) strips = case strips of
			[] -> pure $ coordMapStrip (\hex a -> f c hex a) last
			(strip : rs) -> let
					c' = Map.union c (asContext strip)
				in
					coordMapStrip (\hex a -> f c' hex a) last
						: go (Map.dropWhileAntitone (\(Hex x' _) -> x' <= lastX - 1) c') strip rs

mapWithContextM :: Monad m => (Map Hex a -> Hex -> a -> m b) -> [HexStrip a] -> m [HexStrip b]
mapWithContextM f = go mempty (HexStrip 0 [])
	where
		go c last@(HexStrip lastX _) strips = case strips of
			[] -> pure <$> coordMapStripM (\hex a -> f c hex a) last
			(strip : rs) -> let
					c' = Map.union c (asContext strip)
				in
					(:)
						<$> coordMapStripM (\hex a -> f c' hex a) last
						<*> go (Map.dropWhileAntitone (\(Hex x' _) -> x' <= lastX - 1) c') strip rs

asContext :: HexStrip a -> Map Hex a
asContext (HexStrip x ys) = Map.fromList $ (\(y, a) -> (Hex x y, a)) <$> ys

coordMapStrip :: (Hex -> a -> b) -> HexStrip a -> HexStrip b
coordMapStrip f (HexStrip x ys) = HexStrip x $ (\(y, a) -> (y, f (Hex x y) a)) <$> ys

coordMapStripM :: Monad m => (Hex -> a -> m b) -> HexStrip a -> m (HexStrip b)
coordMapStripM f (HexStrip x ys) = do
	ys' <- (\(y, a) -> liftSnd $ second (f $ Hex x y) $ (y, a)) `mapM` ys
	return $ HexStrip x ys'




sliceHexes :: [(Hex, a)] -> [HexStrip a]
sliceHexes hexes =
		fmap (\vals -> toStrip vals)
		. groupBy (\a b -> (== EQ) $ (cmpX `on` fst) a b)
		. sortBy ((cmpX `on` fst) <> (cmpY `on` fst))
			$ hexes
	where
		toStrip :: [(Hex, a)] -> HexStrip a
		toStrip hexes = case hexes of
			(Hex x y, a):rs -> HexStrip x $ (y, a) : ((\(Hex _ y, a') -> (y, a')) <$> rs)
			[] -> error "bad group"
		cmpX = (\(Hex x _) (Hex x' _) -> compare x x')
		cmpY = (\(Hex _ y) (Hex _ y') -> compare y y')

-- this ends up reversing the strip, but since the order we go through each strip doesn't actually matter it's fine
fillGaps :: HexStrip a -> HexStrip a
fillGaps (HexStrip x ys) = HexStrip x $ case ys of
	[] -> []
	h:rs -> snd $ foldl' fill (h, [h]) rs
	where
		fill :: ((Int, a), [(Int, a)]) -> (Int, a) -> ((Int, a), [(Int, a)])
		fill ((py, v), hexes) (ny, v') = ((ny, v'), (ny, v') : new <> hexes)
			where
				new = (,) <$> innerRange py ny <*> pure v
		innerRange lo hi = takeWhile (< hi) . drop 1 $ [lo..]

decoration :: RandomGen g => (a -> Rand g [TRecord b]) -> V3 Float -> a -> Rand g [TRecord b]
decoration gen base a = do
	decor <- gen a
	return $ translateRecord base <$> decor

decorateStrip :: RandomGen g => ((SlopeHex, a) -> Rand g [TRecord b]) -> HexStrip [(SlopeHex, a)] -> Rand g [TRecord b]
decorateStrip gen (HexStrip x ys) = fmap concat $ uncurry decorate `mapM` concat (liftSnd <$> ys)
	where
		decorate y (sl@(SlopeHex h _), a) = decoration gen basePt (sl, a)
			where
				basePt = let
						(rx, ry) = render $ Hex x y
						rh = heightVal h
					in V3 rx rh ry

data SlopeHex = SlopeHex
	{ base :: Int
	, slopes :: (Slope, Slope, Slope, Slope, Slope, Slope)
	}
	deriving (Eq, Ord, Show, Read)
data Slope = Flat | Up | Down
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

toFlatSlope :: Int -> SlopeHex
toFlatSlope h = SlopeHex h (Flat, Flat, Flat, Flat, Flat, Flat)
{-
-- weirdly when using this it would seem to generate values that were all Up/Down slopes rarely, despite that seeming to be totally impossible.
toRandomSlope :: RandomGen g => Int -> Rand g SlopeHex
toRandomSlope h = do
	opt <- fromMaybe (error "bad select") <$> randomFromList
		[ [Flat, Flat, Down, Flat, Flat, Up]
		, [Flat, Flat, Up, Flat, Flat, Flat]
		, [Flat, Up, Up, Flat, Down, Down]
		, [Flat, Flat, Flat, Down, Flat, Flat]
		, [Flat, Up, Up, Flat, Flat, Flat]
		, [Flat, Down, Down, Flat, Flat, Flat]
		]
	rot <- liftRand $ randomR (0, 5)
	return $ SlopeHex h $ to6ple . Utility.Shuffle.rotate rot $ opt
	{-
	sls <- iterateMN rwalk 6 Flat
	rot <- liftRand $ randomR (0, 5)
	return $ SlopeHex h . to6ple . Utility.Shuffle.rotate rot $ sls
	-}
	where
		{-
		-}
		to6ple :: [a] -> (a,a,a,a,a,a)
		to6ple ps = case ps of
			p0:p1:p2:p3:p4:p5:_ -> (p0, p1, p2, p3, p4, p5)
			_ -> error "bad random select"

rwalk :: RandomGen g => Slope -> Rand g Slope
rwalk sl = fromMaybe (error "bad walk") <$> case sl of
	Flat -> randomFromList [Flat, Up, Down]
	Up -> randomFromList [Flat, Up]
	Down -> randomFromList [Flat, Down]

iterateMN :: Monad m => (a -> m a) -> Int -> a -> m [a]
iterateMN f = go
	where
		go 0 z = return [z]
		go n z = do
			z' <- f z
			(z :) <$> go (n-1) z'

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f = go
	where
		go z = do
			z' <- f z
			(z :) <$> go z'
-}

-- run slopePolyIndices and then map the indices to TP () and put them in TPoly . Convex
slopeGeometry :: SlopeHex -> [TRecord ()]
slopeGeometry slope@(SlopeHex h (p0, p1, p2, p3, p4, p5)) =
		TPoly . Convex . fmap toPoint <$> slopePolyIndices slope
	where
		height sl = case sl of
			Flat -> h
			Up -> h + 1
			Down -> h - 1
		toPoint :: Int -> TPoint ()
		toPoint ix = TP (V3 0 y 0 + pt ix) ()
			where
				y = heightVal $ height p
				p = case ix of
					0 -> p0
					1 -> p1
					2 -> p2
					3 -> p3
					4 -> p4
					5 -> p5
					x -> error $ "slope " <> show slope <> " produced > 5 index: " <> show (slopePolyIndices slope)
		pt i = pts !! (i `mod` 6)
			where
				pts = (^* 0.5) . (\(x, z) -> V3 x 0 z) . render <$> adjacent 0

slopePolyIndices :: SlopeHex -> [[Int]]
slopePolyIndices slope = tail (slopeJoinIndices slope) <> slopeLevelTriIndices slope

-- because there are only six points, and three are needed to have a surface, then there's only the possibility for one level tri per height level.
slopeLevelTriIndices :: SlopeHex -> [[Int]]
slopeLevelTriIndices (SlopeHex h (p0, p1, p2, p3, p4, p5)) =
		flatTri <> upTri <> downTri
	where
		flatTri = if length flats >= 3
			then pure $ ((`mod` 6) . fst) <$> flats
			else []
		upTri = case fst <$> ups of
			-- list has at least 3 elements
			ups'@(ix:_:_:_) -> fromMaybe []
				. fmap (pure . fmap (`mod` 6) . init)
				. snd
				. foldr circlePoints (False, Just [])
					$ ups' <> [ix+6]
			_ -> []
		downTri = case fst <$> downs of
			downs'@(ix:_:_:_) -> fromMaybe []
				. fmap (pure . fmap (`mod` 6) . init)
				. snd
				. foldr circlePoints (False, Just [])
					$ downs' <> [ix+6]
			_ -> []
		(flats, rests) = partition ((Flat ==) . snd) $ zip [0..] [p0,p1,p2,p3,p4,p5]
		(ups, downs) = partition ((Up ==) . snd) $ rests

slopeJoinIndices :: SlopeHex -> [[Int]]
slopeJoinIndices slope@(SlopeHex h (p0, p1, p2, p3, p4, p5)) = case rotatedPs of
	(ix, Flat):rs -> fmap reverse $ snd $ foldl' foo ((ix, Flat), [[]]) rs
	_ -> error $ "bad slope (couldn't find base-height point?!) " <> show slope <> " / " <> show rotatedPs
	where
		foo :: ((Int, Slope), [[Int]]) -> (Int, Slope) -> ((Int, Slope), [[Int]])
		foo ((lastIx, lastHeight), cur:polys) (ix, height) = if lastHeight /= height
			then if height == Flat
				then ((ix, height), ([]:(addTwo ix lastIx cur):polys))
				else ((ix, height), ((addTwo ix lastIx cur):polys))
			else ((ix, height), cur:polys)
		foo (_, []) _ = error "bad slope geometry generation"
		addTwo a b [] = [a,b]
		addTwo a b (h:rs)
			| b == h = a:h:rs
			| otherwise = a:b:h:rs
		ps = zip [0..] [p0, p1, p2, p3, p4, p5]
		rotatedPs = let
				ds = dropWhile ((/= Flat) . snd) ps
				ts = takeWhile ((/= Flat) . snd) ps
			in ds <> ts <> (case ds of
				h:_ -> [h]
				_ -> error $ "no base-height corner on tile; this is a geometry error: " <> show slope <> " / " <> show rotatedPs)

circlePoints :: Int -> (Bool, Maybe [Int]) -> (Bool, Maybe [Int])
circlePoints _ (_, Nothing) = (True, Nothing)
circlePoints ix (hadGap, Just (last:rs)) = if ix == last - 1
		then (hadGap, Just $ ix:last:rs)
	else if hadGap
		then (True, Nothing)
	else (True, Just $ ix:last:rs)
circlePoints ix (hadGap, Just []) = (hadGap, Just [ix])
{-
planes :: [V3 Float] -> [V3 Float]
planes (o:p1:p2:rs) = (normalize $ (p1 - o) `cross` (p2 - o)) : planes (p1:p2:rs)
planes _ = []
-}
-- given a hex strip, generate polygon data for the hexes themselves, ignoring any spandrel generation
tiles :: HexStrip (SlopeHex, a) -> [TRecord a]
tiles (HexStrip x ys) = uncurry tpoly =<< ys
	where
		tpoly y (sl, c) = fmap (const c) . translateRecord center
				<$> slopeGeometry sl
			where
				center = (\(x, z) -> V3 x 0 z) . render $ Hex x y

tile :: Hex -> SlopeHex -> a -> [TRecord a]
tile hex sl a = fmap (const a) . translateRecord center <$> slopeGeometry sl
	where
		center = (\(x, z) -> V3 x 0 z) . render $ hex

newtype ConnectiveLayer = CL { getCL :: Int }
	deriving (Eq, Ord, Show, Read)

-- a spandel here is the extra triangle between each triad of non-aligned hexes. it receives two values for each corner from each hex (or conversely recieves one edge from each hex). a spandrel without at least one value for each corner is ignored.
data Spandrel a = Spandrel Int ConnectiveLayer
	-- ()s in place to make it clear which two points each hex gives information about
	(Maybe (a, a, ())) -- from hex a
	(Maybe ((), a, a)) -- from hex b
	(Maybe (a, (), a)) -- from hex c
	deriving (Eq, Ord, Show, Read, Functor)

instance Semigroup (Spandrel a) where
	Spandrel ix l as bs cs
		<> Spandrel ix' l' as' bs' cs' =
				Spandrel (if ix == ix' then ix else error "trying to merge invalid spandrels")
					(if l == l' then l' else error "trying to merge spandrels on separate layers")
					(p as as')
					(p bs bs')
					(p cs cs')
			where
				p ma mb = case (ma, mb) of
					(Nothing, Nothing) -> Nothing
					(Just _, Nothing) -> ma
					(Nothing, Just _) -> mb
					_ -> error "duplicate spandrel entries"

-- given a hex strip and a list of incomplete 'back' spandrels, return 1. the completed list of 'back' spandrels and 2. an incomplete list of 'front' spandrels, which will be the 'back' spandrels for the next strip
{-
patches :: [Spandrel a] -> HexStrip a -> ([Spandrel a], [Spandrel a])
patches backs (HexStrip x ys) =
		( mergeSpandrels $ concat $ backs : backs'
		, mergeSpandrels $ concat fronts
		)
	where
		(backs', fronts) = unzip $
			uncurry availableSpandrelData . (\(y, a) -> (const a, Hex x y)) <$> ys
-}
patches :: [Spandrel (Int, a)] -> HexStrip [(ConnectiveLayer, (SlopeHex, a))] -> ([Spandrel (Int, a)], [Spandrel (Int, a)])
patches backs (HexStrip x ys) =
		( mergeSpandrels $ concat $ backs : backs'
		, mergeSpandrels $ concat fronts
		)
	where
		(backs', fronts) = unzip $
			uncurry3 availableSpandrelData
			. (\(y, (con, (sl, c))) -> (sliceSlope sl c, con, Hex x y))
				<$> concat (fmap liftSnd ys)
		sliceSlope (SlopeHex h (p0, p1, p2, p3, p4, p5)) c i = case i of
			0 -> (h + hval p0, c)
			1 -> (h + hval p1, c)
			2 -> (h + hval p2, c)
			3 -> (h + hval p3, c)
			4 -> (h + hval p4, c)
			5 -> (h + hval p5, c)
			x -> error $ "patches got index of " <> show x
		hval sl = case sl of
			Flat -> 0
			Up -> 1
			Down -> (-1)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

spanStrip :: Monoid b => (Int -> Int -> a -> (b, b)) -> HexStrip a -> (b, b)
spanStrip f (HexStrip x ys) = go ys
	where
		go [] = (mempty, mempty)
		go ((y, a):rs) = let
				(back, front) = f x y a
				(backs, fronts) = go rs
			in (back <> backs, front <> fronts)

{- a -> [(ConnectiveLayer, SlopeHex)]
-}

averageHeight :: Map Hex (Int, a) -> Hex -> (Int, a) -> (Int, a)
averageHeight adjs hex (height, c) =
	let
		filledAdjs = fmap fst $ catMaybes $ Map.lookup <$> adjacent hex <*> pure adjs
		average vs = sum vs `div` length vs
	in (average $ height : filledAdjs, c)

mergeSpandrels :: [Spandrel a] -> [Spandrel a]
mergeSpandrels ss =
		fmap mergeIx .
		groupBy (\a b -> (== EQ) $ (priorityCmp <> ixCmp) a b) .
		sortBy (priorityCmp <> ixCmp) $
			ss
	where
		priorityCmp :: Spandrel a -> Spandrel a -> Ordering
		priorityCmp (Spandrel _ p _ _ _) (Spandrel _ q _ _ _) = compare p q
		ixCmp :: Spandrel a -> Spandrel a -> Ordering
		ixCmp (Spandrel i _ _ _ _) (Spandrel j _ _ _ _) = compare i j
		mergeIx [] = error "bad group"
		mergeIx (h:rs) = foldr (<>) h rs

hasThreeEdges :: Spandrel a -> Bool
hasThreeEdges (Spandrel _ _ (Just _) (Just _) (Just _)) = True
hasThreeEdges _ = False
hasTwoEdges :: Spandrel a -> Bool
hasTwoEdges (Spandrel _ _ a b c) = (== 2) . length $ catMaybes [const () <$> a, const () <$> b, const () <$> c]

ptsA :: Maybe (a, a, ()) -> [a]
ptsA Nothing = []
ptsA (Just (a, b, _)) = [a, b]

ptsB :: Maybe ((), a, a) -> [a]
ptsB Nothing = []
ptsB (Just (_, b, c)) = [b, c]

ptsC :: Maybe (a, (), a) -> [a]
ptsC Nothing = []
ptsC (Just (a, _, c)) = [c, a]

edgesEqualHeight :: (a -> a -> Bool) -> Spandrel a -> Maybe a
edgesEqualHeight eq (Spandrel _ _ as bs cs) = case ptsA as <> ptsB bs <> ptsC cs of
	[] -> Nothing
	h:rs -> if all (eq h) rs
		then Just h
		else Nothing

seamless :: (a -> a -> Bool) -> Spandrel a -> Maybe (a, a, a)
seamless eq spandrel@(Spandrel _ _ as bs cs) = case ptsA as <> ptsB bs <> ptsC cs of
	a1:a2:b2:b3:c3:c1:_ -> if (a1 `eq` c1) && (b2 `eq` a2) && (c3 `eq` b3)
		then Just (a1, b2, c3)
		else Nothing
	_ -> Nothing

seamless2 :: (a -> a -> Bool) -> Spandrel a -> Maybe (a, a, a)
seamless2 eq spandrel@(Spandrel _ _ as bs cs) = case (as, cs, bs) of
	(Just (a1, a2, ()), Just (c1, (), c3), Nothing) -> if a1 `eq` c1
		then Just (a1, a2, c3)
		else Nothing
	(Just (a1, a2, ()), Nothing, Just ((), b2, b3)) -> if b2 `eq` a2
		then Just (a1, b2, b3)
		else Nothing
	(Nothing, Just (c1, (), c3), Just ((), b2, b3)) -> if c3 `eq` b3
		then Just (c1, b2, c3)
		else Nothing
	_ -> Nothing

{- a spandrel may have multiple resolutions here, since e.g. with edges 0 0, 0 1, 1 1:
	0 0, 0 1 are seamless with 1, 1 as the outlier
	0 1, 1 1 are seamless with 0 0 as the outlier
currently it just picks the first two that match
-}
twoEdgesSeamless :: (a -> a -> Bool) ->  Spandrel a -> Maybe ((a, a, a), ((a, a), (a, a), SpandrelSide))
twoEdgesSeamless eq (Spandrel _ _ as bs cs) = case (,,) <$> as <*> bs <*> cs of
	Just ((a1, a2, ()), ((), b2, b3), (c1, (), c3)) ->
		if a1 `eq` c1 && ((b2 `neq` a2) || (c3 `neq` b3)) -- b off
			then Just ((a1, a2, c3), ((b2, b3), (a2, c3), B))
		else if b2 `eq` a2 && ((a1 `neq` c1) || (c3 `neq` b3)) -- c off
			then Just ((a1, b2, b3), ((c3, c1), (b3, a1), C))
		else if c3 `eq` b3 && ((a1 `neq` c1) || (b2 `neq` a2)) -- a off
			then Just ((c1, b2, c3), ((a1, a2), (c1, b2), A))
		else Nothing
	Nothing -> Nothing
	where
		neq a b = not $ eq a b


splitThreeEdges :: Show a => (a -> a -> Ordering) -> Spandrel a -> Maybe ((a, a), SpandrelSide, (a, a), SpandrelSide, (a, a), SpandrelSide)
splitThreeEdges cmp (Spandrel _ _ as bs cs) = case (,,) <$> as <*> bs <*> cs of
	Just ((a1, a2, ()), ((), b2, b3), (c1, (), c3)) -> let
			edges = zip [[a1, a2], [b2, b3], [c3, c1]] [A,B,C] -- should this be A C B :V
			medges = maybeSort (\m n -> distinct cmp (fst m) (fst n)) edges
		in case medges of
			Just [([low1, low2], lowSide), ([mid1, mid2], midSide), ([high1, high2], highSide)] ->
				Just
					( (low1, low2), lowSide
					, (mid1, mid2), midSide
					, (high1, high2), highSide
					)
			_ -> Nothing
	Nothing -> Nothing

{-



		Just (a, b, c) -> case sortBy (cmp `on` fst) $ zip [a,b,c] [A,B,C] of
			[(low, lowSide), (mid, midSide), (high, highSide)] -> Just
				(low, lowSide, mid, midSide, high, highSide)
			_ -> Nothing
		Nothing -> Nothing
	where
		ae = listToMaybe $ ptsA as
		be = listToMaybe $ ptsB bs
		ce = listToMaybe $ ptsC cs



-}

-- this is probably not efficient but look. i'm using it on lists that are three entries long.
maybeSort :: Show a => (a -> a -> Maybe Ordering) -> [a] -> Maybe [a]
maybeSort cmp = go
	where
		go [] = Just []
		go [v] = Just [v]
		go (a:b:rs) = case a `cmp` b of
			Nothing -> Nothing
			Just EQ -> interleave [a, b] <$> go rs
			Just LT -> interleave [a, b] <$> go rs
			Just GT -> interleave [b, a] <$> go rs
		interleave as [] = as
		interleave [] bs = bs
		interleave (a:as) (b:bs) = case a `cmp` b of
			Just EQ -> a : b : interleave as bs
			Just LT -> a : interleave as (b:bs)
			Just GT -> b : interleave (a:as) bs
			Nothing -> trace ("got criss-cross edge (i think) @ " <> show (a:as) <> " vs. " <> show (b:bs)) $ a : b : interleave as bs -- error $ "got Nothing on interleave: " <> show (a:as) <> " " <> show (b:bs)

-- are all of `as` lower-or-equal or higher-or-equal to all of `bs`?
distinct :: (a -> a -> Ordering) -> [a] -> [a] -> Maybe Ordering
distinct cmp = go
	where
		go (a:as) (b:bs) = shareSign =<< sequence [Just $ a `cmp` b, go (a:as) bs, go as (b:bs)]
		go _ [] = Just EQ
		go [] _ = Just EQ

shareSign :: [Ordering] -> Maybe Ordering
shareSign signs = case partition (== LT) . filter (/= EQ) $ signs of
	([], []) -> Just EQ
	([], _) -> Just GT
	(_, []) -> Just LT
	_ -> Nothing

data SpandrelSide = A | B | C
	deriving (Eq, Ord, Show, Read)

instance Enum SpandrelSide where
	toEnum i = case i `mod` 3 of
		0 -> A
		1 -> B
		_ -> C
	fromEnum s = case s of
		A -> 0
		B -> 1
		C -> 2

data SpandrelPoint = PointingUp | PointingDown
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

spandrelPoint :: Spandrel a -> SpandrelPoint
spandrelPoint (Spandrel ix _ _ _ _) = if ix `mod` 2 == 0 then PointingUp else PointingDown

basis :: Int -> Int -> (SpandrelPoint, V3 Float)
basis x y2 = (point, base)
	where
		point = if y2 `mod` 2 == 0 then PointingUp else PointingDown
		y = y2 `div` 2
		base = (\(V2 x z) -> V3 x 0 z) $ asV2 $ Hex x y

asV2 :: Hex -> V2 Float
asV2 = uncurry V2 . render

heightVal :: Int -> Float
heightVal v = fromIntegral v * (-7)

tri :: SpandrelPoint -> (Int, a) -> (Int, a) -> (Int, a) -> [TRecord a]
tri facing (aHeight, a) (bHeight, b) (cHeight, c) =
	pure . TPoly . Convex $
		[ TP (V3 p0x p0y p0z) a
		, TP (V3 p1x p1y p1z) b
		, TP (V3 p2x p2y p2z) c
		]
	where
		p0y = heightVal aHeight
		p1y = heightVal bHeight
		p2y = heightVal cHeight
		(V2 p0x p0z, V2 p1x p1z, V2 p2x p2z) =
			( sidePt facing A
			, sidePt facing B
			, sidePt facing C
			)

-- generate a join. if any of the height values are equal, automatically reduce complexity, instead of generating a squashed tri
join :: SpandrelPoint -> SpandrelSide -> (Int, a) -> (Int, a) -> (Int, a) -> (Int, a) -> [TRecord a]
join facing side (lo1, lo1c) (lo2, lo2c) (hi1, hi1c) (hi2, hi2c) =
		pure $ TPoly $ Convex $
			(if hi1 == lo1
				then
					[ TP (V3 p0x (heightVal hi1) p0z) hi1c
					]
				else
					[ TP (V3 p0x (heightVal lo1) p0z) lo1c
					, TP (V3 p0x (heightVal hi1) p0z) hi1c
					])
			<>
			(if hi2 == lo2
				then
					[ TP (V3 p1x (heightVal hi2) p1z) hi2c
					]
				else
					[ TP (V3 p1x (heightVal hi2) p1z) hi2c
					, TP (V3 p1x (heightVal lo2) p1z) lo2c
					])
	where
		(V2 p0x p0z) = sidePt facing side
		(V2 p1x p1z) = sidePt facing (succ side)

sidePt :: SpandrelPoint -> SpandrelSide -> V2 Float
sidePt point side = case (point, side) of
	(PointingUp, A) -> 0.5 * (asV2 $ adjacent (Hex 0 0) !! 5)
	(PointingUp, B) -> 0.5 * (asV2 $ adjacent (Hex 0 0) !! 4)
	(PointingUp, C) -> 0.5 * (asV2 $ Hex (-2) (-1))
	(PointingDown, A) -> 0.5 * (asV2 $ Hex (-1) 1)
	(PointingDown, B) -> 0.5 * (asV2 $ adjacent (Hex 0 0) !! 0)
	(PointingDown, C) -> 0.5 * (asV2 $ adjacent (Hex 0 0) !! 5)


-- given a spandrel, generate its geometry (or nothing, if it's incomplete)
{-
todo:
	if a spandrel has three edges contributed,
		and all edges are seamless,
			then make a seamless spandrel
		and two edges are seamless,
			then make the spandrel that high and add a vertical join (up or down) to the third edge
		and they all differ in height,
			then make the spandrel the height of the middle value, and add two vertical joins, up and down, to the other two edges
	if a spandrel has two edges contributed,
		and they're seamless
			then make the spandrel that high
		and they're of differing heights
			then make the spandrel the height of the lower one, and add a vertical join (up) to the higher one -- this isn't implemented in this current code
-}
-- this doesn't actually cover all possible cases, so sometimes it will totally fail to generate seamless geometry. the remaining cases aren't difficult, they're just not handled
spandrelTri :: Show a => Int -> Spandrel (Int, a) -> [TRecord a]
spandrelTri x spandrel@(Spandrel y2 _ _ _ _) = translateRecord base <$> case first
			[ seamless ((==) `on` fst) spandrel
			, seamless2 ((==) `on` fst) spandrel
			] of
		Just (a, b, c) -> tri point a b c
		Nothing -> if hasThreeEdges spandrel
			then case twoEdgesSeamless ((==) `on` fst) spandrel of -- this doesn't do a crosscut check
				Just ((a, b, c), ((other1, other2), (shared1, shared2), side)) ->
					tri point a b c
					<> join point side shared1 shared2 other1 other2
				Nothing -> case splitThreeEdges (compare `on` fst) spandrel of
					Just ((lo1, lo2), lowSide, (midz, midsz), midSide, (hi1, hi2), highSide) ->
						let
							-- the points returned correspond to different indices depending on which side the middle is. the points will be the extant data for that edge, so, since edge A has points `(a, a, ()` then 1 and 2 points are the first two, and so on for the other two edges
							(mid1, mid2, mid3) = case midSide of
								A -> (midz, midsz, midssz)
								B -> (midssz, midz, midsz)
								C -> (midsz, midssz, midz)
							-- FIXME: need to synthesize a more reasonable third point for the mid tri...
							-- (something like, the highest(??) point from the two other edges, maybe)
							midssz = if fst midz > fst midsz
								then midz else midsz
							(lomid1, lomid2) = matchPts mid1 mid2 mid3 lowSide
							(himid1, himid2) = matchPts mid1 mid2 mid3 highSide
						in
							tri point mid1 mid2 mid3
							<> join point lowSide lomid1 lomid2 lo1 lo2
							<> join point highSide himid1 himid2 hi1 hi2
					Nothing -> []
			else []
	where
		matchPts a b c s = case s of
			A -> (a, b)
			B -> (b, c)
			C -> (c, a)
		(point, base) = basis x y2
		first [] = Nothing
		first (Just a:_) = Just a
		first (Nothing:rs) = first rs


{-
spandrels have an indexing:
	for 'back' spandrels:
		'top' spandrel index = y*2
		'left' spandrel index = y * 2 + 1
		'right' spandrel index = y * 2 - 1
	for 'front' spandrels:
		same as 'back' spandrels but +1 for every index
-}
availableSpandrelData :: (Int -> a) -> ConnectiveLayer -> Hex -> ([Spandrel a], [Spandrel a])
availableSpandrelData f con (Hex x y) = (,)
	-- back spandrels
	[ backleft   (f 0) (f 5) -- height points 0 5
	, backcenter (f 5) (f 4) -- height points 5 4
	, backright  (f 4) (f 3) -- height points 4 3
	]
	-- front spandrels
	[ frontright  (f 3) (f 2) -- height points 3 2
	, frontcenter (f 2) (f 1) -- height points 2 1
	, frontleft   (f 1) (f 0) -- height points 1 0
	]
	where
		-- this is understandably a bit byzantine but this should be correct
		backleft a b = Spandrel (y * 2 + 1) con
			Nothing
			(Just ((), a, b))
			Nothing
		backcenter a b = Spandrel (y * 2) con
			(Just (a, b, ()))
			Nothing
			Nothing
		backright a b = Spandrel (y * 2 - 1) con
			(Just (a, b, ()))
			Nothing
			Nothing
		frontleft a b = Spandrel (y * 2 + 2) con
			Nothing
			(Just ((), a, b))
			Nothing
		frontcenter a b = Spandrel (y * 2 + 1) con
			Nothing
			Nothing
			(Just (b, (), a))
		frontright a b = Spandrel (y * 2) con
			Nothing
			Nothing
			(Just (b, (), a))

{-

even spandrels are up-pointing; odd ones are down-pointing


the x,0 up spandrel touches:
	0,0 (bottom edge)
	-1,0 (left edge)
	-1,-1 (right edge)
the x,0 down spandrel touches:
	0,0 (top edge)
	1,0 (right edge)
	1,1 (left edge)

'back' spandrels:
x,0 up spandrel:
	  x,0 #5 / x-1, 0 #2
	  x,0 #4 / x-1,-1 #1
	x-1,0 #3 / x-1,-1 #0

x,1 down spandrel: (to the left)
	x,0 #0 /   x,1 #3
	x,0 #5 / x-1,0 #2
	x,1 #4 / x-1,0 #1

x,-1 down spandrel: (to the right)
	 0, 0 #4 / -1,-1 #1
	 0, 0 #3 /  0,-1 #0
	-1,-1 #2 /  0,-1 #5

'front' spandrels:
	same as the back spandrels for the next 


-}

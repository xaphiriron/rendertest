module Shape.Graph
	( baseGraph
	, renderGraph

	, graphAsRecords
	) where

import Data.Monoid
import Data.Maybe
import qualified Data.Set as Set
import Data.Char

import Data.Graph.Inductive
import Data.Graph.Inductive.Utility (gmapM)
import Data.GraphGrammar

import Linear.V2
import Linear.V3
import Linear.Vector
import Data.Turtle (TPoint(..), TRecord(..))
import qualified Data.Turtle as T
import Data.Color

import Shape.Hex hiding (rotate, (^*))
import Shape.Hex.ShapeEmbedding

import Utility.Rand
import Utility.Shuffle (shuffle)

import Generator.Landscape
import Generator.Dungeon
import Generator.DungeonGen

baseGraph :: RandomGen g => Int -> Int -> g -> IO (Gr (GenNode Shape (Zone, [(Item, Hex)])) (GenEdge (Hex, Hex)))
baseGraph n percent g = do
	let requiredTilesUsed = fx n * percent `div` 100
	let containingShape = HSize 0 n
	let genMap = evalRand g $ do
		startEmbedding <- do
			r <- liftRand $ randomR (0, 5 :: Int)
			return $ case r of
				0 -> \i -> HSize (Hex   0    i ) 2
				1 -> \i -> HSize (Hex   i    i ) 2
				2 -> \i -> HSize (Hex   i    0 ) 2
				3 -> \i -> HSize (Hex   0  (-i)) 2
				4 -> \i -> HSize (Hex (-i) (-i)) 2
				5 -> \i -> HSize (Hex (-i)   0 ) 2
				_ -> error "bad gen"
		grammar <- dungeonGeneratorLoop
			(\_ -> do
				t <- liftRand $ randomR (0, 18 :: Int)
				return $ case t of
					0 -> HSize 0 0
					1 -> HSize 0 0
					2 -> Sub 0 0 $ Line 1
					3 -> Sub 0 0 $ Line 1
					4 -> Sub 0 0 $ Line 2
					5 -> Sub 0 0 $ Line 3
					6 -> Sub 0 0 $ Triangle 1
					7 -> Sub 0 0 $ Triangle 1
					8 -> Sub 0 0 $ Triangle 2
					9 -> Sub 0 0 $ Triangle 3
					10 -> Sub 0 0 $ Diamond 1 1
					11 -> Sub 0 0 $ Diamond 1 2
					12 -> Sub 0 0 $ Diamond 2 1
					13 -> Sub 0 0 $ Diamond 2 2
					14 -> Sub 0 0 $ Trapezoid 2 1
					15 -> Sub 0 0 $ Trapezoid 3 1
					16 -> Sub 0 0 $ Trapezoid 3 2
					17 -> HSize 0 1
					18 -> HSize 0 1
					_ -> error "bad size")
			startEmbedding
			(shapeEmbedding (shapeFullyInside $ containingShape) _p embedGenNode)
			(totalSize requiredTilesUsed)
		graph <- generate grammar
		case graph of
			Left err -> return $ Left err
			Right gr -> do
				gr' <- solidifyLocations gr
				return $ Right gr'
	case genMap of
		Left err -> error $ show err
		Right graph -> return graph

graphAsRecords :: Gr (GenNode Shape (Zone, [(Item, Hex)])) (GenEdge (Hex, Hex)) -> [TRecord ColorRGB]
graphAsRecords = renderGraph
	(drawNode (const $ RGB8 0xcc 0xcc 0xcc) (const $ RGB8 0x11 0x11 0x11) 0.33)
	(drawEdge (\_ _ e -> e) (const $ RGB8 0x90 0x90 0x90))

renderGraph :: Graph gr => (n -> [r]) -> (n -> n -> e -> [r]) -> gr n e -> [r]
renderGraph drawNode drawEdge gr = nodes <> edges
	where
		nodes = drawNode =<< (snd <$> labNodes gr)
		edges = (fromMaybe (error "bad edge") . \(n, m, e) -> drawEdge
			<$> lab gr n
			<*> lab gr m
			<*> pure e) =<< labEdges gr

data Item = Treasure | KeyLoc Char | SwitchLoc Char | TeleporterLoc Char
	deriving (Eq, Ord, Show, Read)

solidifyLocations :: (DynGraph gr, RandomGen g) => gr (GenNode Shape Zone) (GenEdge ()) -> Rand g (gr (GenNode Shape (Zone, [(Item, Hex)])) (GenEdge (Hex, Hex)))
solidifyLocations gr = gmapM setPositions gr
	where
		setPositions :: RandomGen g => Context (GenNode Shape Zone) (GenEdge ()) -> Rand g (Context (GenNode Shape (Zone, [(Item, Hex)])) (GenEdge (Hex, Hex)))
		setPositions (in_, n, nData, out) = do
			a' <- zonePick nData
			newIn <- (\(e, m) ->
				let mData = fromMaybe (error "missing node") $ lab gr m
				in (,) <$> (edgePick mData nData e) <*> pure m) `mapM` in_
			newOut <- (\(e, m) ->
				let mData = fromMaybe (error "missing node") $ lab gr m
				in (,) <$> (edgePick nData mData e) <*> pure m) `mapM` out
			return (newIn, n, a', newOut)

		zonePick :: RandomGen g => GenNode Shape Zone -> Rand g (GenNode Shape (Zone, [(Item, Hex)]))
		zonePick n@(ND s ks d (Zone i t)) = do
			hs <- liftRand $ shuffle $ containedInShape s
			return $ (ND s ks d (Zone i t, if length items > length hs
				then error $ unwords ["too many items in shape: ", show n]
				else zip items hs
				))
			where
				items = let tf = if t then (Treasure :) else id
					in tf $ (\item -> case item of
						HasKey (Key k) -> KeyLoc k
						HasSwitch (Switch s) -> SwitchLoc s
						HasTeleporter (Teleporter t) -> TeleporterLoc t) <$> ks

		edgePick :: RandomGen g => GenNode Shape Zone -> GenNode Shape Zone -> GenEdge () -> Rand g (GenEdge (Hex, Hex))
		edgePick a b (ED l d _) = do
			hs <- randomFromList $ edgePairLocs a b
			return $ case hs of
				Nothing -> error $ "non-adjacent shapes in graph: " <> show a <> " / " <> show b
				Just pair -> ED l d pair

edgePairLocs :: GenNode Shape a -> GenNode Shape a -> [(Hex, Hex)]
edgePairLocs (ND ap _ _ _) (ND bp _ _ _) = Set.toList $ shapeConnections ap bp

hexLinePath :: Hex -> Hex -> [V2 Float]
hexLinePath start end = [V2 ax ay, V2 bx by]
	where
		(ax, ay) = render start
		(bx, by) = render end

drawNode :: (Zone -> ColorRGB) -> (Zone -> ColorRGB) -> Float -> GenNode Shape (Zone, [(Item, Hex)]) -> [TRecord ColorRGB]
drawNode zoneColor borderColor borderOffset (ND shape _ _ (n, is)) =
	base : (border <> (renderItem =<< is))
	where
		renderItem :: (Item, Hex) -> [TRecord ColorRGB]
		renderItem (i, hex) = case i of
			Treasure -> pure $ toRecord (RGB8 0xff 0xdd 0x11) (-2)
				$ triangle 8 0 $ uncurry V2 $ render hex
			KeyLoc k -> pure $ toRecord (keydrawf k) (-2)
				$ uncurry V2 <$> outlineShape 0.22 (HSize hex 0)
			SwitchLoc s -> pure $ toRecord (keydrawf s) (-2)
				$ error "no switch rendering"
			TeleporterLoc t -> asLineLoop (teledrawf t) (-2)
				$ uncurry V2 <$> outlineShape 0.18 (HSize hex 0)
		shapeLines = uncurry V2 <$> outlineShape borderOffset shape
		base :: TRecord ColorRGB
		base = toRecord (zoneColor n) 0 $ shapeLines
		border :: [TRecord ColorRGB]
		border = asLineLoop (borderColor n) 1 shapeLines

asLines :: ColorRGB -> Float -> [V2 Float] -> [TRecord ColorRGB]
asLines zero y pts = zipWith (\p1 p2 -> toRecord zero y [p1, p2]) pts (tail pts)

asLineLoop :: ColorRGB -> Float -> [V2 Float] -> [TRecord ColorRGB]
asLineLoop _ _ [] = []
asLineLoop zero y (h:rs) = asLines zero y (h:rs <> [h])

drawEdge :: (a -> a -> GenEdge (Hex, Hex) -> c) -> (c -> ColorRGB) -> GenNode Shape a -> GenNode Shape a -> GenEdge (Hex, Hex) -> [TRecord ColorRGB]
drawEdge edgeType drawf n m e = edgeShapes
	where
		edgeLine = case e of
			ED _ _ (start, end) -> hexLinePath start end
		ev = edgeType (label n) (label m) e
		(start, end) = case e of
			(ED _ _ (s, e)) -> (uncurry V2 . render $ s, uncurry V2 . render $ e)
		edgeShapes :: [TRecord ColorRGB]
		edgeShapes = case e of
			ED l _ _ -> case l of
				Default ->
					pure $ toRecord (drawf ev) 1 $ edgeLine
				Open ->
					pure $ toRecord (drawf ev) 1 $ edgeLine
				-- add colored | shape
				LockedWith (Key k) ->
					[ toRecord (drawf ev) 1 $ edgeLine
					, toRecord (keydrawf k) 0 $ centeredRect 12 4 (line 0.5) (rotation Towards)
					]
				-- add |> shape
				LockedFromSide side ->
					[ toRecord (drawf ev) 1 $ edgeLine
					, toRecord (drawf ev) 1
						$ strokeline 12 (if side == Towards then line 0.4 else line 0.6) (rotation side)
					]
					<> asLines (drawf ev) 1 (caret 12 4 (if side == Towards then line 0.6 else line 0.4) (rotation side))
				-- add > shape
				OneWay side ->
					[ toRecord (drawf ev) 1 $ edgeLine ]
					<> asLines (drawf ev) 1 (caret 12 4 (line 0.5) (rotation side))
				UnlockedBySwitch _ -> error "no 'unlocked by switch' render"
				LockedBySwitch _ -> error "no 'locked by switch' render"
				Impassible -> []
			where
				line :: Float -> V2 Float
				line t = start + ((end - start) ^* t)
				baseRotation = ((pi/2) +) $ (\(V2 x y) -> atan2 y x) $ end - start
				rotation s = if s == Towards then baseRotation else baseRotation + pi


toRecord :: a -> Float -> [V2 Float] -> TRecord a
toRecord zero y [pt] = TVertex $ toPoint zero y pt
toRecord zero y [p1, p2] = TLine (toPoint zero y p1) (toPoint zero y p2)
toRecord zero y pt = TPoly $ toPoint zero y <$> pt

toPoint :: a -> Float -> V2 Float -> TPoint a
toPoint zero y (V2 x z) = TP (V3 x y z) zero


centeredRect :: Float -> Float -> V2 Float -> Float -> [V2 Float]
centeredRect width height center angle = pts
	where
		pts = ((center +) . rotate angle) <$>
			[ V2 (-width / 2) (-height / 2)
			, V2 (width / 2) (-height / 2)
			, V2 (width / 2) (height / 2)
			, V2 (-width / 2) (height / 2)
			, V2 (-width / 2) (-height / 2)
			]

caret :: Float -> Float -> V2 Float -> Float -> [V2 Float]
caret width height center angle = pts
	where
		pts = ((center +) . rotate angle) <$>
			[ V2 (-width / 2) (height / 2)
			, V2 0 (-height / 2)
			, V2 (width / 2) (height / 2)
			]

strokeline :: Float -> V2 Float -> Float -> [V2 Float]
strokeline width center angle = pts
	where
		pts = ((center +) . rotate angle) <$>
			[ V2 (-width / 2) 0
			, V2 (width / 2) 0
			]

triangle :: Float -> Float -> V2 Float -> [V2 Float]
triangle r angle center = pts
	where
		pts = ((center +) . rotate angle) <$>
			(take 3 $ iterate (rotate rad) $ V2 0 r)
		rad = 120 / 180 * pi

rotate :: Float -> V2 Float -> V2 Float
rotate rad (V2 x y) = V2 x' y'
	where
		x' = x * cos rad - y * sin rad
		y' = y * cos rad + x * sin rad


teledrawf :: Char -> ColorRGB
teledrawf = ci

keydrawf :: Char -> ColorRGB
keydrawf = ci

ci :: Char -> ColorRGB
ci c = cis !! i
	where
		i = ord c - ord 'a'
		cis =
			[ (RGB8 0x11 0x11 0xff)
			, (RGB8 0x11 0xff 0x11)
			, (RGB8 0xff 0x11 0x11) -- close to (planned) enemy color
			, (RGB8 0x11 0x11 0x11)
			, (RGB8 0x11 0xff 0xff)
			, (RGB8 0x33 0x33 0x33)
			, (RGB8 0xff 0x11 0xff)
--			, (RGB8 0x66 0x66 0x66) -- blends poorly with floor triangles
--			, (RGB8 0xff 0xff 0x00) -- close to color treasure
--			, (RGB8 0x99 0x99 0x99) -- blends poorly with floor triangles
			, (RGB8 0x11 0x99 0xff)
--			, (RGB8 0xcc 0xcc 0xcc) -- blends poorly with floor triangles
			, (RGB8 0x11 0xff 0x99)
--			, (RGB8 0xff 0xff 0xff) -- too hard to see
			, (RGB8 0x99 0xff 0x11)
			, (RGB8 0xff 0x99 0x11)
			, (RGB8 0xff 0x11 0x99)
			, (RGB8 0x99 0x11 0xff)
			, (RGB8 0x11 0x11 0x88)
			, (RGB8 0x11 0x88 0x11)
			, (RGB8 0x88 0x11 0x11) -- close to (planned) enemy color
			, (RGB8 0x11 0x88 0x88)
			, (RGB8 0x88 0x11 0x88)
			, (RGB8 0x88 0x88 0x11)
			, (RGB8 0x88 0x88 0xff)
			, (RGB8 0x88 0xff 0x88)
			, (RGB8 0xff 0x88 0x88)
			, (RGB8 0x88 0xff 0xff)
			, (RGB8 0xff 0x88 0xff)
--			, (RGB8 0xff 0xff 0x88) -- probably too hard to see
			] <> repeat (RGB8 0xff 0xff 0xff)

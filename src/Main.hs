{-
    Copyright © 2018 xax

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Prelude hiding ((.), id)

import Control.Arrow (first, second, (***))
import Control.Applicative
import Control.Category
import Control.Monad (unless, (<=<), foldM, join)
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Graphics.GPipe hiding (Format(..), FlatVFloat(..), (^*))
import qualified Graphics.GPipe as GP (Format(..))
import "GPipe-GLFW" Graphics.GPipe.Context.GLFW (Key(..), KeyState(..), ModifierKeys(..), MouseButton(..), MouseButtonState(..))
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Data.Maybe (fromMaybe, mapMaybe, isJust, listToMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Merge.Lazy as M
import Data.List (foldl', partition, sortBy)
import Data.Foldable
import Data.Function hiding ((.), id)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Word (Word8)

import qualified Data.Set as Set

import Data.Int (Int16)

import Linear.Vector

import Data.Color
import Data.PossibilitySpace hiding (bounds)
import Data.Turtle hiding (Mode(Line))
import Utility.SVGRender (SVGDrawData(..))
import Utility.Tuple
import Shape
-- import Shape.House
-- import Shape.Coin
import Shape.Plant
import Shape.HexField
import GLRenderer

import Shape.Tree

import Reactive.Banana.Combinators (Event, Behavior, MonadMoment, filterJust, accumB, valueB, valueBLater, (<@>), stepper, never, filterE, filterApply)
import Reactive.Banana.Frameworks (EventNetwork, reactimate, actuate, compile, fromAddHandler, fromPoll, mapEventIO)
import Control.Event.Handler

import Input.Types
import Render.Font
import Render.Object

import Utility.Rand
import Utility.Shuffle
import Data.Graph.Inductive.Utility

import Data.GraphGrammar hiding (flip)
import Data.Graph.Inductive
import Generator.DungeonGen
import Shape.Hex hiding (render, (^*))
import Shape.Hex.ShapeEmbedding (shapeEmbedding)
import Generator.Dungeon hiding (Key)
import qualified Generator.Dungeon as Dungeon
import Generator.HeightLandscape

import Debug.Trace as D (trace)

windowSize :: V2 Float
windowSize = V2 800 600

windowWidth :: Float
windowWidth = case windowSize of
	V2 x _ -> x

windowHeight :: Float
windowHeight = case windowSize of
	V2 _ y -> y

z :: V4 a -> a
z (V4 x y z_ w) = z_

xyz :: V4 a -> V3 a
xyz (V4 x y z _) = V3 x y z

w :: V3 a -> a -> V4 a
w (V3 x y z) w = V4 x y z w

data RawCameraInput = Cam
	{ camActive :: Bool
	, rotationInput :: V2 Float
	, storedRotation :: V2 Float
	, translationInput :: V2 Float
	, storedTranslation :: V2 Float
	}

buttonMap b = case b of
	MouseButton'1 -> Just LeftClick
	MouseButton'2 -> Just RightClick
	_ -> Nothing

main :: IO ()
main = runContextT GLFW.defaultHandleConfig foo

{-
render single terrain hex
	render layered terrain hex
	figure out connected overhangs
place plant on tile
	give plant l-system 'check environment' action & branch
		make 'check environment' action actually read terrain data from hex grid
	make plant autogenerate comprehensible l-system from data spec

-}

foo :: ContextT GLFW.Handle os IO ()
foo = do
	win <- newWindow (WindowFormatColorDepth GP.RGBA8 GP.Depth16)
		$ (GLFW.defaultWindowConfig "Hello world!")
			{ GLFW.configWidth = floor windowWidth
			, GLFW.configHeight = floor windowHeight
			}

	addRawGLFWHandler <- setRawCallbacks win

	camVar <- liftIO $ newMVar $ (False, Cam False 0 0 0 0)

	-- render state readers/writers. the writer is used in the event network, and the reader is sent to `loop` where it's polled to check for new render state
	(write, read) <- liftIO . atomically $ do
		write <- newBroadcastTChan
		read <- dupTChan write
		return (write, read)

	blank :: Texture2D os (GP.Format RGBAFloat) <- newTexture2D GP.RGBA8 (V2 1 1) 1
	writeTexture2D blank 0 (V2 0 0) (V2 1 1) [V4 1 1 1 1 :: V4 Float]

	network <- liftIO $ compile $ do
		rawEv <- fromAddHandler addRawGLFWHandler -- Event RawGLFW
		windowState <- accumB (InputState (V2 0 0))
			$ filterJust $ (\ev -> case ev of
					CursorPosEvent x y -> Just (\s -> s { mousePosition = floor <$> V2 x y })
					_ -> Nothing)
				<$> rawEv
		let fauxEvs = filterJust $ convertGLFW <$> rawEv <#> windowState

		let randomIndex = generateRandomIndex $ renderMap svgToColor (geneticPlantsRender 0)

		randFunc <- mapEventIO (\_ -> randomIndex)
			$ fmap (const ()) $ filterE (\ev -> ev == KeyDown Key'R) fauxEvs
		startIx <- liftIO $ randomIndex
		randIx <- stepper startIx randFunc
		number <- mdo
			let updates = filterJust $ (\ev ix -> case ev of
				KeyDown Key'Space -> Just $ first (+ 1) -- grow one step
				KeyDown Key'R -> Just $ const 1 *** const ix -- reset back to default, but with a new seed
				_ -> Nothing
				) <$> fauxEvs <#> randIx
			number <- stepper (1, startIx) $ updates <#> number
			return number

		let plevs = (\ev (steps, ix) -> case ev of
			-- KeyUp Key'R -> modifyMVar_ camVar $ pure . first (const True)
			-- these just both do a rerender
			KeyUp Key'R -> do
					putStrLn $ "(on reset) steps: " <> show steps <> "; ix: " <> show ix
					liftIO $ atomically $ writeTChan write (1, Replace $ do
						plantPolys <- liftIO $ fmap (fmap $ mapRecord (* 2))
							$ generateSpace ix $ renderMap svgToColor (geneticPlantsRender steps)
						plantMesh <- generateRenderObject blank plantPolys
						return [plantMesh])
			KeyUp Key'Space -> do
					putStrLn $ "(on increment) steps: " <> show steps <> "; ix: " <> show ix
					liftIO $ atomically $ writeTChan write (1, Replace $ do
						plantPolys <- liftIO $ fmap (fmap $ mapRecord (* 2))
							$ generateSpace ix $ renderMap svgToColor (geneticPlantsRender steps)
						plantMesh <- generateRenderObject blank plantPolys
						return [plantMesh])
			_ -> return ()) <$> fauxEvs <#> number
		let camera = cameraControl camVar <$> fauxEvs
		reactimate $ camera <> plevs

	liftIO $ actuate network

	viewmatrixBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1
	uimatrixBuffer :: Buffer os (Uniform (V4 (B4 Float))) <- newBuffer 1

	writeBuffer uimatrixBuffer 0 . pure $ identity

	shader :: CompiledShader os (Texture2D os (GP.Format RGBAFloat), PrimitiveArray Triangles (B4 Float, B4 Float, B2 Float), Bool) <- compileShader $ do
		primitiveStream <- toPrimitiveStream $ \(_, v, _) -> v

		samp <- newSampler2D (\(t, _, _) ->
			( t
			, SamplerFilter Nearest Nearest Nearest Nothing
			, (pure ClampToBorder, V4 1 0 1 1)
			))
		let sampleTexture = sample2D samp SampleAuto Nothing Nothing

		matrix <- getUniform $ \(_, _, u) -> if u
			then (viewmatrixBuffer, 0)
			else (uimatrixBuffer, 0)
		let matrixF' = (matrix !*)
		let primitiveStream2 =
			fmap ((\(p, c, uv) -> (matrixF' p, (c, uv))))
				$ primitiveStream

		fragmentStream <- withRasterizedInfo
				(\(c, uv) r -> (c * (sampleTexture uv), z $ rasterizedFragCoord r))
			<$> rasterize
				(const (Front, ViewPort (V2 0 0) (V2 (floor windowWidth) (floor windowHeight)), DepthRange 0 1))
				primitiveStream2
		drawWindowColorDepth
			(\(_, _, w) ->
				( win
				, ContextColorOption
					(BlendRgbAlpha
						(FuncAdd, FuncAdd)
						(BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors One One)
						(V4 1 0 1 1)
					)
					(V4 True True True True)
				, if w then DepthOption Less True
						else DepthOption Always False
				)
			)
			fragmentStream

	chex <- loadTexture "img/texture.png"

	font <- constructFont $ \page -> case page of
		Unicode 0 -> Just ("img/font-medium.png", V2 21 27, 32, 256)
		Symbols -> Just ("img/symbols.png", V2 24 24, 4, 8)
		_ -> Nothing

	worldField <- liftIO $ genWorldField

	liftIO . atomically $ do
		writeTChan write (0, Replace $ do
			let worldPolys = generateMesh terrainColor worldField
			worldMesh <- generateRenderObject blank worldPolys
			return [worldMesh])
		writeTChan write (1, Replace $ do
			plantPolys <- liftIO $ fmap (fmap $ mapRecord (* 2))
				$ generateRandomSpace $ renderMap svgToColor (geneticPlantsRender 15)
			plantMesh <- generateRenderObject blank plantPolys
			return [plantMesh])

	-- this doesn't take the network (anymore) b/c we don't actually use it, but probably it should so that it can pause or reactivate itself, or w/e
	loop camVar read shader win viewmatrixBuffer
		mempty
		(0 :: Float)

setRawCallbacks :: Window os c ds -> ContextT GLFW.Handle os IO (AddHandler RawGLFW)
setRawCallbacks win = do
	(addHandler, fire) <- liftIO $ newAddHandler

	GLFW.setMouseButtonCallback win $ Just $ \button state mod -> do
		fire $ MouseButtonEvent button state mod
	GLFW.setCursorPosCallback win $ Just $ \x y -> do
		fire $ CursorPosEvent x y
	GLFW.setKeyCallback win $ Just $ \key i s mod -> do
		fire $ KeyAction key i s mod
	GLFW.setCharCallback win $ Just $ \char -> do
		fire $ CharAction char
	GLFW.setWindowCloseCallback win $ Just $ do
		fire $ WindowClose

	return addHandler

{-
-- the event network would have a MVar (Map Integer (Update [RenderObject ...])) that it would push updates to. sure that would mean it might generate geometry for objects that never get rendered (due to being updated again before any renders happen) but uh for now that's fine. idk if making it `() -> [RenderObject ...]` would be any better?
-}
data UpdateState a = Replace a | Delete
	deriving (Eq, Ord, Show, Read, Functor)

instance Foldable UpdateState where
	foldMap f u = case u of
		Delete -> mempty
		Replace a -> f a

instance Traversable UpdateState where
	sequenceA f = case f of
		Delete -> pure Delete
		Replace a -> Replace <$> a

data Terrain = LooseSand | PackedSand | Dust | Ash | Dirt | Rock | Obsidian | RockGravel | ObsidianGravel
	deriving (Eq, Ord, Show, Read)

terrainColor :: Terrain -> ColorRGBA
terrainColor t = case t of
	LooseSand   -> RGBA8 0xff 0xff 0xdd 0xff
	PackedSand  -> RGBA8 0xcc 0xcc 0x99 0xff
	Obsidian    -> RGBA8 0x66 0x22 0x66 0xff
	_           -> RGBA8 0xff 0x00 0xff 0xff

-- presumably we want terrain stacks or smth here, not just a single terrain value
genWorldField :: IO (HexField Terrain)
genWorldField = do
	seed <- fst . random <$> getStdGen
	putStrLn $ "generating new map with seed " <> show seed
	let (field, _) = runRand (mkStdGen seed) $ testMap
	return field

testMap :: RandomGen g => Rand g (HexField Terrain)
testMap = do
	-- blah blah random generator
	return $ HexField
		[ HexStrip 0 [(0, PackedSand)]
		]

generateRenderObject :: Texture2D os (GP.Format RGBAFloat) -> [TRecord (ColorRGBA, V2 Float)] -> ContextT GLFW.Handle os IO (RenderObject os (B4 Float, B4 Float, B2 Float))
generateRenderObject blank polys = do
	let (translucentPolys, solidPolys) = partition transRec polys
	solidVertices <- sequenceA $ fmap (\buf -> Chunk Nothing buf World) <$> glUVSurfaces solidPolys
	translucentVertices <- sequenceA $ fmap (\buf -> Chunk Nothing buf World) <$> glUVSurfaces translucentPolys
	return $ RenderObject blank $ catMaybes [solidVertices, translucentVertices]

randomWorldRenderObject :: Texture2D os (GP.Format RGBAFloat) -> ContextT GLFW.Handle os IO (RenderObject os (B4 Float, B4 Float, B2 Float))
randomWorldRenderObject blank = do
	polys <- liftIO $ generateRandomThing
	generateRenderObject blank polys

transRec :: TRecord (ColorRGBA, x) -> Bool
transRec r = case r of
	TVertex p -> transPt p
	TLine p q -> transPt q || transPt q
	TPoly sh -> case sh of
		Convex pts -> any transPt pts
		Complex pts _ -> any transPt pts

transPt :: TPoint (ColorRGBA, x) -> Bool
transPt (TP _ (c, _)) = isTranslucent c

isTranslucent :: ColorRGBA -> Bool
isTranslucent (RGBA8 _ _ _ a) = a < 255

isOpaque = not . isTranslucent

generateRandomIndex :: RenderSpace a b c -> IO Integer
generateRandomIndex renderSpace = do
	seed <- fst . random <$> getStdGen
	putStrLn $ "generating new thing with seed " <> show seed
	let (i, seed') = runRand (mkStdGen seed) $ liftRand $ randomR (0, count (space renderSpace) - 1)
	setStdGen seed'
	return i

generateSpace :: Integer -> RenderSpace a b ColorRGBA -> IO [TRecord (ColorRGBA, V2 Float)]
generateSpace i renderSpace = do
	let lights =
		[ (RGBA8 0xff 0xf8 0xf0 0xff, normalize $ V3 2 3 1)
		, (RGBA8 0x5f 0x80 0x90 0xff, negate $ normalize $ V3 2 3 1)
--		, (RGB8 0x88 0x84 0x77, normalize $ V3 1 0.5 2)
--		, (RGB8 0x88 0x84 0x77, normalize $ V3 2 0.5 (-1))
		] :: [(ColorRGBA, V3 Float)]
	return $ fromMaybe (error "bad selection") $ pickSpaceUVs lights renderSpace i

generateRandomSpace :: RenderSpace a b ColorRGBA -> IO [TRecord (ColorRGBA, V2 Float)]
generateRandomSpace renderSpace = do
	i <- generateRandomIndex renderSpace
	generateSpace i renderSpace

generateRandomThing :: IO [TRecord (ColorRGBA, V2 Float)]
generateRandomThing = do
	seed <- fst . random <$> getStdGen
	putStrLn $ "generating new map with seed " <> show seed
	-- let fakeGraphSpace = evalRand (mkStdGen seed) $ randomGraphField 1200
	let (fakeGraphSpace, r') = runRand (mkStdGen seed) $ randomLandscapeField
		Plain
		[Forest, Plain, Desert, Badland, Wasteland, Mesa, Hills, Mountain, Swamp, Marsh, Lake]
		5000
	let (i, r'') = runRand r' $ liftRand $ randomR (0, count (space fakeGraphSpace) - 1)
	setStdGen r''

	let lights =
		[ (RGBA8 0xff 0xf8 0xf0 0xff, normalize $ V3 2 3 1)
		, (RGBA8 0x5f 0x80 0x90 0xff, negate $ normalize $ V3 2 3 1)
--		, (RGB8 0x88 0x84 0x77, normalize $ V3 1 0.5 2)
--		, (RGB8 0x88 0x84 0x77, normalize $ V3 2 0.5 (-1))
		] :: [(ColorRGBA, V3 Float)]
	return $ fromMaybe (error "bad selection") $ pickSpaceUVs lights fakeGraphSpace i

mapButton :: GLFW.MouseButton -> Maybe Button
mapButton b = case b of
	MouseButton'1 -> Just LeftClick
	MouseButton'2 -> Just RightClick
	_ -> Nothing

convertGLFW :: RawGLFW -> InputState -> Maybe FauxGLFW
convertGLFW ev pos = case ev of
	MouseButtonEvent button state _ -> case (,) state <$> mapButton button of
		Just (MouseButtonState'Pressed, side) -> Just $ MouseDown side (mousePosition pos)
		Just (MouseButtonState'Released, side) -> Just $ MouseUp side (mousePosition pos)
		_ -> Nothing
	KeyAction k _ state _ -> case state of
		KeyState'Pressed -> Just $ KeyDown k
		KeyState'Released -> Just $ KeyUp k
		_ -> Nothing
	CharAction c -> Just $ Typed c
	CursorPosEvent x y -> Just $ MouseMove $ round <$> V2 x y
	WindowClose -> Just Close

eapply :: Behavior a -> Event (a -> b) -> Event b
eapply b ef = ((\v -> ($ v)) <$> b) <@> ef

infixl 4 <#>
(<#>) :: Event (a -> b) -> Behavior a -> Event b
(<#>) = flip eapply

keyfunc :: MVar RawCameraInput -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyfunc move k i s mod = case k of
	Key'Escape -> return () {- runContextT GLFW.defaultHandleConfig $ do
		Just () <- GLFW.setWindowShouldClose win True
		return () -}
	_ -> case moveVector k of
		Just v -> do
			modifyMVar_ move $ case s of
				KeyState'Pressed -> pure . (\c -> c { translationInput = translationInput c + v })
				KeyState'Released -> pure . (\c -> c { translationInput = translationInput c - v })
				KeyState'Repeating -> return
			return ()
		Nothing -> return ()

cameraControl :: MVar (Bool, RawCameraInput) -> FauxGLFW -> IO ()
cameraControl cam ev = case ev of
	KeyDown k -> case moveVector k of
		Just v -> modifyMVar_ cam
			$ pure . second (\c -> c { translationInput = translationInput c + v })
		Nothing -> return ()
	KeyUp k -> case moveVector k of
		Just v -> modifyMVar_ cam
			$ pure . second (\c -> c { translationInput = translationInput c - v })
		Nothing -> return ()
	MouseDown RightClick xy -> modifyMVar_ cam
			$ pure . second (\c -> c
				{ camActive = True
				, rotationInput = relPos xy
				})
	MouseUp RightClick _ ->
		modifyMVar_ cam $ pure . second (\c -> c { camActive = False })
	MouseMove xy ->
		modifyMVar_ cam $ pure . second (\c -> c
			{ rotationInput = relPos xy
			})
	_ -> return ()
	where
		relPos :: V2 Int -> V2 Float
		relPos pt = ((fromIntegral <$> pt) - half) / half
			where
				half = windowSize / 2

moveVector :: Key -> Maybe (V2 Float)
moveVector k = case k of
	Key'Up -> Just $ V2 0 1
	Key'Down -> Just $ V2 0 (-1)
	Key'Left -> Just $ V2 1 0
	Key'Right -> Just $ V2 (-1) 0
	_ -> Nothing

{-
rendering a frame would require placing all relevant data in storage and then generating the commands to render it; rerendering a frame would just mean calling the commands again

(you can't change uniforms _within_ the render calls, or uh shouldn't?)

input is constantly being processed in the background, but that means that `keyfunc` needs some big state value to actually report events with (or maybe a minimal 'game state and real event actions' structure, w/ an event channel so it can turn raw keypresses into 'real' game events)

with gpipe, input is only processed during the `swapWindowBuffers` call (i think). this means we'll only ever see the event network produce stuff between frames.
each loop should render a frame and optionally try polling the network's mvar output states. if it does poll them, then it might need to update parts of the renderobject data. currently there's no way to do that (render objects are untagged and consist of an enormous chunk of totally unlabeled geometry), but that would be useful for e.g., loading and unloading geometry. something like, an mvar would contain state data to generate polys, with each state having possible values of:
	'clean', which wouldn't need anything
	'dirty', which would need to replace an existing render chunk
	'new', which would need to produce a new render chunk
	'dead', which would need to remove an existing render chunk
or something like that, and then there would need to be some way of communicating which extant render id (if any) is tied to a given render object. so _preparing to render a frame_ would require parsing that list to update the render chunks, but rerendering a frame would just involve going down the render list rendering everything
-}

collect :: TChan a -> STM [a]
collect ch = do
	mv <- tryReadTChan ch
	case mv of
		Just v -> (v :) <$> collect ch
		Nothing -> return []

updateCache :: Ord k => Map k (UpdateState v) -> Map k v -> Map k v
updateCache = M.merge
	(M.mapMaybeMissing $ \k wv -> case wv of
		Replace v -> Just v -- no value to replace, so just add it
		Delete -> Nothing -- no value to delete, so ignore it
		)
	M.preserveMissing
	(M.zipWithMaybeMatched $ \k u e -> case u of
		Replace v -> Just v
		Delete -> Nothing
		)

loop :: (ContextColorFormat c, DepthRenderable ds, Fractional b, Color c Float ~ V4 b) =>
	MVar (Bool, RawCameraInput)
	-> TChan (Integer, UpdateState (ContextT GLFW.Handle os IO [RenderObject os (B4 Float, B4 Float, B2 Float)])) -- sorry about the type
	-> (((Texture2D os (GP.Format RGBAFloat)), PrimitiveArray Triangles (B4 Float, B4 Float, B2 Float), Bool) -> Render os ())
	-> Window os c ds
	-> Buffer os (Uniform (V4 (B4 Float)))
	-> Map Integer [RenderObject os (B4 Float, B4 Float, B2 Float)]
	-> Float
	-> ContextT GLFW.Handle os IO ()
loop cameraVar readRender shader win viewmatrixBuffer renderObjs = step
	where
		step t = do
			Just t' <- liftIO $ fmap realToFrac <$> GLFW.getTime
			let elapsed = t' - t

			liftIO $ modifyMVar_ cameraVar
				$ pure . second (\cam -> let
						(V2 rx ry) = storedRotation cam
					in cam
						{ storedRotation = if camActive cam
							then storedRotation cam + (rotationInput cam ^* elapsed ^* 2)
							else storedRotation cam
						-- rotate this by storedrotation or w/e
						, storedTranslation = storedTranslation cam
							+ (Main.rotate (-rx) (normalize (translationInput cam)) ^* elapsed ^* 480)
						})
			V2 tx ty <- liftIO $ storedRotation . snd <$> readMVar cameraVar
			V2 cx cy <- liftIO $ storedTranslation . snd <$> readMVar cameraVar
			makeNewGeometry <- liftIO $ fst <$> readMVar cameraVar
			liftIO $ modifyMVar_ cameraVar $ pure . first (const False)

			updatedGeometry <- join . liftIO . atomically . fmap (traverse $ liftSnd . fmap sequenceA)
				$ collect readRender

			-- see this could be in the event network, right? maybe?
			writeBuffer viewmatrixBuffer 0 . pure
				$ perspective (deg2rad 90) (windowWidth / windowHeight) 10 300 !*!
					lookAt (V3 60 (-60) 0) (V3 0 0 0) (V3 0 (-1) 0) !*!
					mkTransformation (axisAngle (V3 0 0 (-1)) ty) 0 !*!
					mkTransformation (axisAngle (V3 0 1 0) tx) 0 !*!
					translationMatrix (V3 cy 0 cx)


			render $ do
				clearWindowColor win (V4 0.9 0.8 0.7 1.0 :: Fractional b => V4 b)
				clearWindowDepth win 1
				(\(RenderObject blank chunks) -> renderAction blank chunks) `mapM_` fold renderObjs
			swapWindowBuffers win

			closeRequested <- GLFW.windowShouldClose win
			{- if ...
					then loop cameraVar shader win viewmatrixBuffer newGeometry t'
					else step t'
			-}
			newGeometry <- if makeNewGeometry
				then Just <$> randomWorldRenderObject (nullTexture $ head $ fromMaybe (error "no world geometry") $ M.lookup 0 renderObjs)
				else pure Nothing

			let newGeometry' = case updatedGeometry of
				[] -> Nothing
				_ -> Just $ updateCache (M.fromList updatedGeometry) renderObjs

			unless (closeRequested == Just True) $ case {- newGeometry -} newGeometry' of
				Nothing -> step t'
				Just m -> loop cameraVar readRender shader win viewmatrixBuffer m t'
				-- Just m -> loop cameraVar shader win viewmatrixBuffer (M.singleton 0 [m]) t'


		renderAction blank chunks = sequence_ $ (\chunk -> do
			vertexArray <- newVertexArray $ vertices chunk
			let primitiveArray = toPrimitiveArray TriangleList vertexArray
			-- okay so instead of this being a single shader thing, it should be a bundle of uniforms and texture sampler data
			shader (fromMaybe blank $ textureMap chunk, primitiveArray, frame chunk == World)
			) <$> chunks
		translationMatrix (V3 x y z) = V4
			(V4 1 0 0 x)
			(V4 0 1 0 y)
			(V4 0 0 1 z)
			(V4 0 0 0 1)

rotate :: Float -> V2 Float -> V2 Float
rotate t (V2 x y) = V2 (x * cos t - y * sin t) (y * cos t + x * sin t)

randomLandscapeField :: RandomGen g => Landscape -> [Landscape] -> Int -> Rand g
	(RenderSpace
		[TRecord (Biome Landscape)]
		(Biome Landscape)
		ColorRGBA
	)
randomLandscapeField starting allowed size = do
	mgr <- landscapeGraph starting allowed size
	let gr = case mgr of
		Left err -> error $ show err
		Right gr -> gr
	recs <- graphMesh gr
	return $ RenderSpace (pure recs) id (const withTranslucentWater)

withTranslucentWater :: Biome Landscape -> ColorRGBA
withTranslucentWater b = case original b of
	Lake -> withAlpha 0xc0 $ biomeColor b
	_ -> withAlpha 0xff $ biomeColor b

graphMesh :: (Graph gr, RandomGen g) => gr Location e -> Rand g [TRecord (Biome Landscape)]
graphMesh = heightShapes
		decor'
		customShapePlacement
		averageHeightAndBiomeExceptingWater
		(\adj h a -> pure . placeSlopes adj h $ a)
		(\b -> flip (,) (landscapeAsBiome Lake) <$> waterHeight b)
	. renderGraph (pure . getHeightShapeColor) (\_ _ _ -> [])
	where
		getHeightShapeColor :: Location -> (Int, Shape, Landscape)
		getHeightShapeColor (Location landscape height shape _) =
			( height
			, shape
			, landscape
			)
		-- lol
		mapTree tree = colorBiome . treeMatColor $ tree
		mapRock rock = colorBiome . rockMatColor $ rock

		decor' (SlopeHex height _, Biome heat moisture _ l _) = case l of
			Forest -> do
				r <- rand
				if r < 0.8
					then fmap (fmap mapTree) <$> tree height heat moisture
				else if r < 0.9
					then fmap (fmap mapTree) <$> grass
				else return []
			Hills -> do
				r <- rand
				if r < 0.2
					then fmap (fmap mapTree) <$> tree height heat moisture
				else if r < 0.5
					then fmap (fmap mapRock) <$> rockType Granite
				else if r < 0.55
					then fmap (fmap mapTree) <$> grass
				else return []
			Mountain -> do
				r <- rand
				if r < 0.15
					then fmap (fmap mapRock) <$> rockType Granite
				else if r < 0.3
					then fmap (fmap mapRock) <$> rockCluster Granite
				else if r < 0.45
					then fmap (fmap mapRock) <$> rockCluster Basalt
				else if r < 0.6
					then fmap (fmap mapRock) <$> rockType Basalt
				else return []
			Badland -> do
				r <- rand
				if r < 0.1
					then fmap (fmap mapRock) <$> rockCluster Sandstone
				else return []
			Desert -> do
				r <- rand
				if r < 0.05
					then fmap (fmap mapRock) <$> rockType Sandstone
				else if r < 0.1
					then fmap (fmap mapTree) <$> tree height heat moisture
				else return []
			Mesa -> do
				r <- rand
				if r < 0.3
					then fmap (fmap mapRock) <$> rockType Sandstone
				else return []
			Plain -> do
				r <- rand
				if r < 0.1
					then fmap (fmap mapTree) <$> tree height heat moisture
				else if r < 0.15
					then fmap (fmap mapRock) <$> rockCluster Granite
				else if r < 0.2
					then fmap (fmap mapRock) <$> rockType Granite
				else if r < 0.6
					then fmap (fmap mapTree) <$> grass
				else return []
			Swamp -> do
				r <- rand
				if r < 0.6
					then fmap (fmap mapTree) <$> tree height heat moisture
				else if r < 0.9
					then fmap (fmap mapTree) <$> grass
				else return []
			Marsh -> do
				r <- rand
				if r < 0.6
					then fmap (fmap mapTree) <$> grass
				else return []
			_ -> return []

		tree :: RandomGen g => Int -> Float -> Float -> Rand g [TRecord TreeMaterial]
		tree height heat moisture = do
			hRoll <- rand
			mRoll <- rand
			let hFix = if hRoll < hPerc
				then hIndex + 1
				else hIndex
			let mFix = if mRoll < mPerc
				then mIndex + 1
				else mIndex
			{-
			let trees =
				[ [none, cherry, palm]
				, [pine, broadleaf, acacia]
				, [cedar, willow, mahogany]
				]
			-}
			let trees =
				[ [pine, broadleaf, palm]
				, [pine, broadleaf, palm]
				, [cedar, broadleaf, palm]
				]
			trees !! clamp 0 2 mFix !! clamp 0 2 hFix
			where
				clamp lo hi v = max lo $ min hi v
				heightHeatDrop = max 0 $ fromIntegral (height - 24) * 0.05
				(hIndex, hPerc) = let raw = (* 3) $ heat - heightHeatDrop
					in (floor raw, raw - (fromIntegral $ floor raw))
				(mIndex, mPerc) = let raw = (* 3) $ moisture
					in (floor raw, raw - (fromIntegral $ floor raw))
				-- none = pure []
				palm = roll $ toRecord palmTrees <$> space palmTrees
				pine = roll $ toRecord pineTrees <$> space pineTrees
				broadleaf = roll $ toRecord broadleafTrees <$> space broadleafTrees
				cedar = roll $ toRecord cedarTrees <$> space cedarTrees

		rockType :: RandomGen g => RockMaterial -> Rand g [TRecord RockMaterial]
		rockType t = roll (toRecord (rocks t) <$> space (rocks t))
		rockCluster :: RandomGen g => RockMaterial -> Rand g [TRecord RockMaterial]
		rockCluster t = roll (toRecord (rockClusters t) <$> space (rockClusters t))
		grass :: RandomGen g => Rand g [TRecord TreeMaterial]
		grass = roll $ toRecord grasses <$> space grasses

customShapePlacement :: RandomGen g => (Int, Shape, Landscape) -> Rand g [(Hex, (Int, Biome Landscape))]
customShapePlacement (height, shape, color) = case color of
	Lake -> pure $ case shape of
		HSize c r -> ((,) <$> ring c r <*> pure (height, landscapeAsBiome Plain))
			<> ((,)
				<$> ring c (r - 1)
				<*> pure (height - 4, (landscapeAsBiome LakeBed) { waterHeight = Just $ height - 1}))
		_ -> error "whoops"
	Swamp -> case shape of
		HSize c r -> do
			let outer = ((,) <$> ring c r <*> pure (height, landscapeAsBiome Swamp))
			let innerHex = ring c (r - 1)
			inner <- (\h -> do
				res <- waterRegion 0.66 height
					(landscapeAsBiome SwampBottom)
					(landscapeAsBiome Swamp)
				return (h, res)) `mapM` innerHex
			return $ outer <> inner
		_ -> error "whoops"
	Marsh -> case shape of
		HSize c r -> do
			let outer = ((,) <$> ring c r <*> pure (height, landscapeAsBiome Marsh))
			let innerHex = ring c (r - 1)
			inner <- (\h -> do
				res <- waterRegion 0.33 height
					(landscapeAsBiome SwampBottom)
					(landscapeAsBiome Marsh)
				return (h, res)) `mapM` innerHex
			return $ outer <> inner
		_ -> error "whoops"
	_ -> pure $ (,) <$> containedInShape shape <*> pure (height, landscapeAsBiome color)

waterRegion :: RandomGen g => Float -> Int -> Biome a -> Biome a -> Rand g (Int, Biome a)
waterRegion waterRate height water nonWater = do
	w <- rand
	return $ if w < waterRate
		then (height - 4, water { waterHeight = Just $ height - 1} )
		else (height, nonWater)

-- yes yes this is an arrow
mergeContexts :: (Map k a -> k -> a -> b) -> (Map k c -> k -> c -> d) -> Map k (a, c) -> k -> (a, c) -> (b, d)
mergeContexts f g m k (a, c) = (f (fst <$> m) k a, g (snd <$> m) k c)

averageHeightAndBiomeExceptingWater :: Map Hex (Int, Biome Landscape) -> Hex -> (Int, Biome Landscape) -> (Int, Biome Landscape)
averageHeightAndBiomeExceptingWater adjs hex (height, c) =
	let
		extantAdjs = catMaybes $ M.lookup <$> adjacent hex <*> pure adjs
		(waters, smooths) = partition (isWater . original . snd) $ extantAdjs
		finalBiome opts = averageBiomes $ c : opts
		-- limit smoothing so it can't possibly smooth below the local water level (this might not work right w/ the new water height values)
		waterLimit = case (waterHeight . snd) `mapMaybe` waters of
			[] -> Nothing
			vs -> Just $ maximum vs
		filledAdjs = fmap fst $ smooths
		average vs = sum vs `div` length vs
		smoothHeight limit adjs = case limit of
			Nothing -> average $ height : adjs
			Just m -> max m $ average $ height : adjs
	in if isWater . original $ c -- below-water tiles will get height smoothed, but they will do so using all their adjacent tiles, whereas above-water tiles ignore below-water tiles
		then
			(if length waters <= 2 -- isolated water points tend to get averaged and sloped weirdly
				then height
				else smoothHeight Nothing $ fst <$> extantAdjs
			, finalBiome $ snd <$> extantAdjs)
		else (smoothHeight waterLimit $ fst <$> smooths, finalBiome $ snd <$> smooths)

isWater :: Landscape -> Bool
isWater l = l `elem` [Lake, LakeBed, SwampBottom, OceanFloor]

data Biome a = Biome
	{ heat :: Float
	, moisture :: Float
	, waterHeight :: Maybe Int
	, original :: a
	, biomeColor :: ColorRGB
	}
	deriving (Eq, Ord, Show, Read)


landscapeAsBiome :: Landscape -> Biome Landscape
landscapeAsBiome l = case l of
	Forest    -> Biome 0.40 0.60 Nothing l $ landscapeColor l
	Plain     -> Biome 0.50 0.40 Nothing l $ landscapeColor l
	Desert    -> Biome 0.80 0.20 Nothing l $ landscapeColor l
	Badland   -> Biome 0.90 0.10 Nothing l $ landscapeColor l
	Wasteland -> Biome 1.00 0.10 Nothing l $ landscapeColor l
	Mesa      -> Biome 0.70 0.20 Nothing l $ landscapeColor l
	Hills     -> Biome 0.35 0.30 Nothing l $ landscapeColor l
	Mountain  -> Biome 0.20 0.20 Nothing l $ landscapeColor l
	Swamp     -> Biome 0.40 0.80 Nothing l $ landscapeColor l
	Marsh     -> Biome 0.40 0.90 Nothing l $ landscapeColor l
	Lake      -> Biome 0.40 0.90 Nothing l $ landscapeColor l

	Coast     -> Biome 0.50 0.50 Nothing l $ landscapeColor l
	Canyon    -> Biome 0.70 0.30 Nothing l $ landscapeColor l
	Tundra    -> Biome 0.10 0.50 Nothing l $ landscapeColor l
	Glacier   -> Biome 0.00 1.00 Nothing l $ landscapeColor l
	Ocean     -> Biome 0.25 1.00 Nothing l $ landscapeColor l

	OceanFloor -> Biome 0.50 1.00 Nothing l $ landscapeColor l
	LakeBed    -> Biome 0.40 0.90 Nothing l $ landscapeColor l
	SwampBottom-> Biome 0.40 0.80 Nothing l $ landscapeColor l

averageBiomes :: [Biome a] -> Biome a
averageBiomes bs = case foldr1 add bs of
		Biome h m w bio (RGB8 r g b) -> Biome (h / cr) (m / cr) w bio $ RGB8 (r `div` c) (g `div` c) (b `div` c)
	where
		add (Biome h m w l (RGB8 r g b)) (Biome h' m' w' _ (RGB8 r' g' b')) =
			Biome (h + h') (m + m') (first w w') l $ RGB8 (r + r') (g + g') (b + b')
		first m n = case (m, n) of
			(Just a, _) -> Just a
			(_, Just b) -> Just b
			_ -> Nothing
		cr = fromIntegral c
		c = length bs

colorBiome :: ColorRGB -> Biome Landscape
colorBiome c = Biome 0 0 Nothing Ocean c


placeSlopes :: Map Hex (Int, Biome Landscape) -> Hex -> (Int, Biome Landscape) -> (SlopeHex, Biome Landscape)
-- placeSlopes _ _ (h, b@(Biome _ _ _ l _)) | isWater l = (toFlatSlope h, b)
placeSlopes adj hex (h, c) = (SlopeHex h $ to6ple . fmap thd3 $ flattenedSlopes, c)
	where
		-- it's reasonable for a tile to have _two_ flat corners, but if it only has one then i figure that'll probably look ugly, since it'll occur in places like the tops of mountains. and zero flats is an actual error state. so we flatten the 2-3 sharpest slopes, to try to give those tiles a better shape.
		flattenedSlopes = case length . filter ((== Flat) . thd3) $ correctedSlopes of
			x | x <= 1 -> let toFlatten = 3 - x
					in
						-- then flatten
						firstN (\(i, _, _) -> (i, 0, Flat)) toFlatten
						-- sort them by largest absolute difference
						. sortBy (flip compare `on` (abs . snd3))
							$ correctedSlopes
				| otherwise -> correctedSlopes
		fst3 (a, _, _) = a
		snd3 (_, b, _) = b
		thd3 (_, _, c) = c
		firstN :: (a -> a) -> Int -> [a] -> [a]
		firstN _ 0 vs = vs
		firstN f n vs = case vs of
			[] -> []
			h:rs -> f h : firstN f (n - 1) rs
		-- we have to sort this b/c the fold can/will end up with an output list like [1,2,3,4,5,0], which causes a lot of off-by-one issues elsewhere, as you might imagine
		correctedSlopes = sortBy (comparing fst3) $ case last looped of
				(wix', _, wslope') -> if wix /= wix'
					then error $ "?! slope values got weird: " <> show wix <> " vs. " <> show wix' <> " in " <> show naiveSlopes
					else if wslope /= wslope'
						then (wix', 0, Flat) : init looped
						else looped
			where
				((wix, _, wslope), looped) = case naiveSlopes of
					h:rs -> foldr (flip foo) (h, []) (h:rs)
					[] -> error ":|"
		foo :: ((Int, Int, Slope), [(Int, Int, Slope)]) -> (Int, Int, Slope) -> ((Int, Int, Slope), [(Int, Int, Slope)])
		foo ((lix, lrel, lslope), ps) (ix, rel, slope) = if invalidDiscontinuity lslope slope
			then if abs rel < abs lrel
				then ((ix, rel, Flat) , (lix, lrel, lslope) : ps)
				else ((ix, rel, slope), (lix, lrel,   Flat) : ps)
			else ((ix, rel, slope), (lix, lrel, lslope): ps)
		invalidDiscontinuity sl1 sl2 = (sl1 == Up && sl2 == Down)
			|| (sl1 == Down && sl2 == Up)
		naiveSlopes :: [(Int, Int, Slope)]
		naiveSlopes = sortBy (comparing fst3)
			$ zipWith (\i adjCoord -> case M.lookup adjCoord adj of
					Nothing -> (i, 0, Flat)
					-- don't slope towards water beds unless this tile is also a water bed, since sloping the shore towards the bed can recess the shore beneath the adjacent water edge, leading to floating water surfaces
					Just (adjH, Biome _ _ _ l _) | isWater l && (not $ isWater $ original c) -> (i, 0, Flat)
					Just (adjH, _) -> let rel = h - adjH
						in (i, rel, naiveSlope rel)
				) [0..5] $ adjacent hex
		naiveSlope :: Int -> Slope
		naiveSlope rel = if rel <= -1
					then Up
				else if rel > 1
					then Down
				else Flat
		to6ple :: Show a => [a] -> (a,a,a,a,a,a)
		to6ple vs = case vs of
			p0:p1:p2:p3:p4:p5:_ -> (p0, p1, p2, p3, p4, p5)
			_ -> error $ "not enough values in list for 6ple: " <> show vs


rand :: RandomGen g => Rand g Float
rand = liftRand $ random
{-
randomGraphField :: RandomGen g => Int -> Rand g
	(RenderSpace
		(Gr (GenNode Shape (Zone, [(Item, Hex)])) (GenEdge (Hex, Hex)))
		Float
	)
randomGraphField size = do
	graph <- generateBasicGraph size
	return $ case graph of
		Left err -> error $ show err
		Right gr -> RenderSpace (pure gr) graphMesh rainbowColor
-}
generateBasicGraph :: RandomGen g => Int -> Rand g
	(Either
		GenError
		(Gr (GenNode Shape (Zone, [(Item, Hex)])) (GenEdge (Hex, Hex)))
	)
generateBasicGraph requiredTilesUsed = do
	startEmbedding <- do
		r <- liftRand $ randomR (0, 5) :: RandomGen g => Rand g Int
		return $ case r of
			0 -> \i -> HSize (Hex   0    i ) 2
			1 -> \i -> HSize (Hex   i    i ) 2
			2 -> \i -> HSize (Hex   i    0 ) 2
			3 -> \i -> HSize (Hex   0  (-i)) 2
			4 -> \i -> HSize (Hex (-i) (-i)) 2
			5 -> \i -> HSize (Hex (-i)   0 ) 2
			_ -> error "bad gen"
	grammar <- dungeonGenerator
		(\_ -> roll $ HSize 0 <$> rangeNum (0, 4))
		startEmbedding
		(shapeEmbedding (const True) _p embedGenNode)
		(totalSize requiredTilesUsed)
	graph <- generate grammar
	case graph of
		Left err -> return $ Left err
		Right gr -> do
			gr' <- solidifyLocations gr
			return $ Right gr'

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
						HasKey (Dungeon.Key k) -> KeyLoc k
						HasSwitch (Switch s) -> SwitchLoc s
						HasTeleporter (Teleporter t) -> TeleporterLoc t) <$> ks

		edgePick :: RandomGen g => GenNode Shape Zone -> GenNode Shape Zone -> GenEdge () -> Rand g (GenEdge (Hex, Hex))
		edgePick a b (ED l d _) = do
			hs <- randomFromList $ edgePairLocs a b
			return $ case hs of
				Nothing -> error $ "non-adjacent shapes in graph: " <> show a <> " / " <> show b
				Just pair -> ED l d pair

data Item = Treasure | KeyLoc Char | SwitchLoc Char | TeleporterLoc Char
	deriving (Eq, Ord, Show, Read)

edgePairLocs :: GenNode Shape a -> GenNode Shape a -> [(Hex, Hex)]
edgePairLocs (ND ap _ _ _) (ND bp _ _ _) = Set.toList $ shapeConnections ap bp

renderGraph :: Graph gr => (n -> [r]) -> (n -> n -> e -> [r]) -> gr n e -> [r]
renderGraph drawNode drawEdge gr = nodes <> edges
	where
		nodes = drawNode =<< (snd <$> labNodes gr)
		edges = (fromMaybe (error "bad edge") . \(n, m, e) -> drawEdge
			<$> lab gr n
			<*> lab gr m
			<*> pure e) =<< labEdges gr

svgToColor :: SVGDrawData -> ColorRGBA
svgToColor s = case (_stroke s, _fill s) of
	(_, Just (RGB8 r g b)) -> RGBA8 r g b 0xff
	(Just (RGB8 r g b), _) -> RGBA8 r g b 0xff
	_ -> RGBA8 0xff 0x00 0xff 0xff

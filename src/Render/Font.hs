module Render.Font
	( Page(..)
	, Sprite(..)
	, Fontsheet(..)
	, Fontpage(..)
	, constructFont
	, generateUVs
	, placeTextString
	) where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Int (Int16)
import Data.Word (Word8)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (foldl')
import Data.Map (Map, fromList)
import qualified Data.Map as M

import GHC.Ptr (plusPtr)

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

import Data.Vector.Storable (Vector(..), Storable, (!))
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable(..))

import Graphics.GPipe hiding (Format(..), (^*))
import qualified Graphics.GPipe as GP (Format(..))
import Codec.Picture
import qualified Codec.Picture as JP

data Page = Symbols | Unicode Integer
	deriving (Eq, Ord, Show, Read)

data Fontsheet os c = Fontsheet
	{ pages :: Map Page (Fontpage os c)
	, bounding :: Sprite
	}
data Fontpage os c = Fontpage
	{ texture :: Texture2D os (GP.Format c)
	, sizes :: Vector Sprite
	, symbolsInPage :: Int
	}

data Sprite = Sprite !Int16 !Int16 !Int16 !Int16
	deriving (Eq, Ord, Show, Read)

bbSprite :: Sprite -> Sprite -> Sprite
bbSprite (Sprite _ _ w h) (Sprite _ _ w' h') = Sprite 0 0 (max w w') (max h h')

instance Storable Sprite where
	sizeOf _ = sizeOf (undefined :: Int16) * 4
	alignment _ = alignment (undefined :: Int16)
	peekElemOff addr i = do
		let base = addr `plusPtr` (i * sizeOf (undefined :: Sprite))
		let offset = sizeOf (undefined :: Int16)
		x <- peekElemOff base 0
		y <- peekElemOff base 1
		w <- peekElemOff base 2
		h <- peekElemOff base 3
		return $ Sprite x y w h
	pokeElemOff addr i (Sprite x y w h) = do
		let base = addr `plusPtr` (i * sizeOf (undefined :: Sprite))
		pokeElemOff base 0 x
		pokeElemOff base 1 y
		pokeElemOff base 2 w
		pokeElemOff base 3 h

constructFont :: (ContextHandler ctx, MonadIO m) =>
	(Page -> Maybe (String, V2 Int, Int, Int)) ->
	ContextT ctx os m (Fontsheet os RGBAFloat)
constructFont pages = let
		keys = Symbols : (Unicode <$> [0..128])
	in do
		pages <- sequence $ keyedPage `mapMaybe` keys
		let bounding = foldr bbSprite (Sprite 0 0 0 0) $ (! 0) . sizes . snd <$> pages
		return $ Fontsheet (fromList pages) bounding
	where
		keyedPage k =
			(\(path, size, maxSymbolsPerRow, maxSymbols) -> do
				page <- scanTexture path size maxSymbolsPerRow maxSymbols
				return (k, page)
				) <$> pages k
		scanTexture :: (ContextHandler ctx, MonadIO m) => String -> V2 Int -> Int -> Int -> ContextT ctx os m (Fontpage os RGBAFloat)
		scanTexture path (V2 symbolMaxWidth symbolHeight) maxSymbolsPerRow maxSymbols = do
			rawTex <- liftIO $ readImage path
			let (size, pxStream) = case rawTex of
				Left err -> error err
				Right (ImageRGBA8 (Image width height px)) ->
					( V2 width height :: V2 Int
					, fmap ((/ 255) . fromIntegral) <$> splitToV4 px :: [V4 Float] -- this might be backwards, since iirc opengl textures start from the bottom row and juicypixels would start from the top
					)
				Right _ -> error "can't handle image type (RGBA8 only)"
			tex <- newTexture2D GP.RGBA8 size 1
			writeTexture2D tex 0 (V2 0 0) size pxStream

			return $ Fontpage tex (V.unfoldr (produceASCIISprite maxSymbols size pxStream) 0) maxSymbols
			where
				-- FIXME: this is definitely not efficient but i can't actually figure out an efficient way to convert a Vector Word8 into a Vector (V4 Word8)
				produceASCIISprite :: Int -> V2 Int -> [V4 Float] -> Int -> Maybe (Sprite, Int)
				produceASCIISprite maxSymbols (V2 w h) pxls i
					| i >= maxSymbols = Nothing
					| otherwise = Just $
						( Sprite
							(fromIntegral $ x)
							(fromIntegral $ y)
							(fromIntegral $ min symbolMaxWidth vUsed)
							(fromIntegral $ symbolHeight)
						, i + 1
						)
					where
						x = symbolMaxWidth * (i `mod` maxSymbolsPerRow)
						y = symbolHeight * (i `div` maxSymbolsPerRow)
						vUsed = countWhileN symbolMaxWidth (\v -> v /= V4 1 0 1 1)
							. drop ((y * w) + x)
								$ pxls
						countWhileN :: Int -> (a -> Bool) -> [a] -> Int
						countWhileN max pred = go 0
							where
								--go :: Int -> [a] -> Int
								go cur vs
									| cur >= max = max
									| otherwise = case vs of
										[] -> cur
										h:rs -> if pred h
											then go (cur + 1) rs
											else cur

generateUVs :: Fontsheet os RGBAFloat -> Int -> [(Page, Int)] -> (V2 Int, Map Page [(V2 Int, V2 Float)])
generateUVs sheet spacing text = second (M.mapWithKey buildPolys) . formLine spacing $ uncurry rawSize `mapMaybe` text
	where
		rawSize :: Page -> Int -> Maybe (Page, V2 Int, V2 Int)
		rawSize p i = case M.lookup p $ pages sheet of
			Just (Fontpage texture sizes max)
				| i < max -> Just $ case sizes ! i of
					Sprite x y w h ->
						( p
						, fromIntegral <$> V2 x y
						, fromIntegral <$> V2 w h
						)
			_ -> Nothing
		formLine :: Int -> [(Page, V2 Int, V2 Int)] -> (V2 Int, Map Page [((V2 Int, V2 Int), (V2 Int, V2 Int))])
		formLine spacing = first (fromMaybe (V2 0 0)) . foldl' buildPosUV (Nothing, mempty)
			where
				buildPosUV :: (Maybe (V2 Int), Map Page [((V2 Int, V2 Int), (V2 Int, V2 Int))])
					-> (Page, V2 Int, V2 Int)
					-> (Maybe (V2 Int), Map Page [((V2 Int, V2 Int), (V2 Int, V2 Int))])
				buildPosUV (prev, file) (p, xy, wh) = let
						base = case prev of
							Nothing -> V2 0 0
							Just base -> base + V2 spacing 0
						newBase = let
								(V2 bx _) = base + wh
								(V2 _ ty) = base
							in V2 bx ty
					in
						( Just newBase
						, M.insertWith (<>) p
							[((base, base + wh), (xy, xy + wh))]
							file
						)
		buildPolys :: Page -> [((V2 Int, V2 Int), (V2 Int, V2 Int))] -> [(V2 Int, V2 Float)]
		buildPolys page rects = buildPoly (fromMaybe (error "incoherent font sheet")
				. M.lookup page $ pages sheet)
			=<< rects

		buildPoly :: Fontpage os RGBAFloat -> ((V2 Int, V2 Int), (V2 Int, V2 Int)) -> [(V2 Int, V2 Float)]
		buildPoly page ((V2 lx ly, V2 hx hy), (V2 lu lv, V2 hu hv)) =
			[ (V2 lx ly, rel $ V2 lu lv)
			, (V2 hx ly, rel $ V2 hu lv)
			, (V2 hx hy, rel $ V2 hu hv)

			, (V2 lx hy, rel $ V2 lu hv)
			, (V2 lx ly, rel $ V2 lu lv)
			, (V2 hx hy, rel $ V2 hu hv)
			]
			where
				(V2 um vm) = fmap fromIntegral . head . texture2DSizes . texture $ page
				rel (V2 u v) = V2 (fromIntegral u / um) (fromIntegral v / vm)

-- have the actual normalization (the `resolution` part) happen in the shader, not here. all this would do is add `px` and convert the values to V3.
placeTextString :: V2 Int -> V2 Int -> (V2 Int, V2 Float) -> (V3 Float, V2 Float)
placeTextString resolution px (V2 x y, uv) = (scalePos $ fromIntegral <$> V3 x y 0, uv)
	where
		scalePos :: V3 Float -> V3 Float
		scalePos pos = case (fromIntegral <$> resolution) / 2 of
			V2 w h -> (\(V3 x y z) -> V3 x (y * (-1)) z)
				. (+ repos)
				. (/ V3 w h 1)
					$ pos
		repos :: V3 Float
		repos = (\(V2 x y) -> V3 x y 0)
			. (subtract 1)
			. (^* 2)
			. scale (V2 0 0) (fromIntegral <$> resolution)
				$ (fromIntegral <$> px)

scale :: Fractional a => V2 a -> V2 a -> V2 a -> V2 a
scale lo hi p = (p - lo) / range
	where
		range = hi - lo

splitToV4 :: Vector Word8 -> [V4 Word8]
splitToV4 = go
	where
		toV4 :: Storable a => Vector a -> V4 a
		toV4 v = V4 (v ! 0) (v ! 1) (v ! 2) (v ! 3)
		go :: Storable a => Vector a -> [V4 a]
		go vs | V.null vs = []
			| otherwise = case V.splitAt 4 vs of
				(h, rs) -> toV4 h : go rs
module Render.Object
	( RenderChunk(..)
	, RenderObject(..)
	, ReferenceFrame(..)
	, loadTexture
	, splitToV4
	, splitToV4'
	) where

import Control.Monad.IO.Class
import Data.Word (Word8)

import Data.Vector.Storable (Vector(..), Storable, (!))
import qualified Data.Vector.Storable as V

import Graphics.GPipe hiding (Format(..), (^*))
import qualified Graphics.GPipe as GP (Format(..))
import Codec.Picture
import qualified Codec.Picture as JP

data RenderChunk os a = Chunk
	{ textureMap :: Maybe (Texture2D os (GP.Format RGBAFloat))
	, vertices :: Buffer os a
	, frame :: ReferenceFrame
	}

data ReferenceFrame = World | UI
	deriving (Eq, Ord, Show, Read)

data RenderObject os a = RenderObject
	{ nullTexture :: Texture2D os (GP.Format RGBAFloat)
	, chunks :: [RenderChunk os a]
	}

loadTexture :: (ContextHandler ctx, MonadIO m) => String -> ContextT ctx os m (Texture2D os (GP.Format RGBAFloat))
loadTexture path = do
	tex <- liftIO $ readImage path
	let (size, pxStream) = case tex of
		Left err -> error err
		Right (ImageRGBA8 (Image width height px)) ->
			( V2 width height :: V2 Int
			, fmap ((/ 255) . fromIntegral) <$> splitToV4 px :: [V4 Float] -- this might be backwards, since iirc opengl textures start from the bottom row and juicypixels would start from the top
			)
		Right _ -> error "can't handle image type (RGBA8 only)"
	chex <- newTexture2D GP.RGBA8 size 1
	writeTexture2D chex 0 (V2 0 0) size pxStream
	return chex

splitToV4' :: Vector Word8 -> Vector (V4 Word8)
splitToV4' = go
	where
		toV4 :: Storable a => Vector a -> V4 a
		toV4 v = V4 (v ! 0) (v ! 1) (v ! 2) (v ! 3)
		go :: Storable a => Vector a -> Vector (V4 a)
		go vs | V.null vs = V.empty
			| otherwise = case V.splitAt 4 vs of
				(h, rs) -> toV4 h `V.cons` go rs

splitToV4 :: Vector Word8 -> [V4 Word8]
splitToV4 = go
	where
		toV4 :: Storable a => Vector a -> V4 a
		toV4 v = V4 (v ! 0) (v ! 1) (v ! 2) (v ! 3)
		go :: Storable a => Vector a -> [V4 a]
		go vs | V.null vs = []
			| otherwise = case V.splitAt 4 vs of
				(h, rs) -> toV4 h : go rs

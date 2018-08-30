{-# LANGUAGE PackageImports #-}

module GLRenderer
	( glSurface
	, glSurfaces
	, glUVSurfaces

	, toGLTris
	) where

import Data.Maybe
import Data.List ((!!))
import Control.Monad

import Graphics.GPipe hiding (RGB8)
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Data.Turtle
import Data.Color

glUVSurfaces :: [TRecord (ColorRGB, V2 Float)] -> Maybe (ContextT GLFW.Handle os IO (Buffer os (B4 Float, B3 Float, B2 Float)))
glUVSurfaces trecs = toGLTris <$> (case posColorUVs =<< trecs of
	[] -> Nothing
	x -> Just x)

glSurfaces :: [TRecord ColorRGB] -> Maybe (ContextT GLFW.Handle os IO (Buffer os (B4 Float, B3 Float)))
glSurfaces trecs = toGLTris <$> (case join $ rawPtLists `mapMaybe` trecs of
	[] -> Nothing
	x -> Just x)

glSurface :: TRecord ColorRGB -> Maybe (ContextT GLFW.Handle os IO (Buffer os (B4 Float, B3 Float)))
glSurface trec = (toGLTris :: [(V4 Float, V3 Float)] -> ContextT GLFW.Handle os IO (Buffer os (B4 Float, B3 Float))) <$> rawPtLists trec

toGLTris :: BufferFormat b => [HostFormat b] -> ContextT GLFW.Handle os IO (Buffer os b)
toGLTris vs = do
	vBuffer <- newBuffer (length vs)
	writeBuffer vBuffer 0 vs
	return vBuffer

rawPtLists :: TRecord ColorRGB -> Maybe [(V4 Float, V3 Float)]
rawPtLists trec = case trec of
	TVertex {} -> Nothing
	TLine (TP v i) (TP v' i') -> Just $ zip
		(v32v4 <$> lineQuads)
		(color2v3f <$>
			[ i, i', i
			, i, i', i'
			])
--			, ("width", A_Float $ V.fromList $ (const 1) <$> [i, i', i, i'])
		where
			diff :: V3 Float
			diff = case v - v' of
				V3 x 0 0 -> V3 0 0 0.0625
				_ -> V3 0.0625 0 0
			lineQuads =
				[ v + diff, v' + diff , v - diff
				, v - diff, v' + diff , v' - diff
				]
	TPoly (Convex []) -> Nothing
	TPoly (Convex vs) -> Just $ (\(TP v i) -> (v32v4 v, color2v3f i)) <$> pts
		where
			pts = clockwiseToTris vs
	TPoly (Complex vs []) -> Nothing
	TPoly (Complex vs ix) -> Just $ (\(TP v i) -> (v32v4 v, color2v3f i)) <$> pts
		where
			pts = (vs !!) <$> ix

posColorUVs :: TRecord (ColorRGB, V2 Float) -> [(V4 Float, V3 Float, V2 Float)]
posColorUVs trec = case trec of
	TVertex {} -> []
	TLine (TP v (i, uv)) (TP v' (i', uv')) -> zip3
		(v32v4 <$> lineQuads)
		(color2v3f <$>
			[ i, i', i
			, i, i', i'
			])
		uvQuads
		where
			diff :: V3 Float
			diff = case v - v' of
				V3 x 0 0 -> V3 0 0 0.0625
				_ -> V3 0.0625 0 0
			uvDiff :: V2 Float
			uvDiff = V2 1 0
			lineQuads =
				[ v + diff, v' + diff , v - diff
				, v - diff, v' + diff , v' - diff
				]
			uvQuads =
				[ uv + V2 1 0, uv' + V2 1 0, uv
				, uv         , uv' + V2 1 0, uv'
				]
	TPoly poly ->
		let tris = case poly of
			Convex [] -> []
			Convex vs -> clockwiseToTris vs
			Complex vs ix -> (vs !!) <$> ix
		in case tris of
			[] -> []
			pts -> (\(TP v (i, uv)) ->
					( v32v4 v
					, color2v3f i
					, uv
					)
				) <$> pts

posColor :: TRecord ColorRGB -> [(V4 Float, V3 Float)]
posColor trec = case trec of
	TVertex {} -> []
	TLine (TP v i) (TP v' i') -> zip
		(v32v4 <$> lineQuads)
		(color2v3f <$>
			[ i, i', i
			, i, i', i'
			])
		where
			diff :: V3 Float
			diff = case v - v' of
				V3 x 0 0 -> V3 0 0 0.0625
				_ -> V3 0.0625 0 0
			lineQuads =
				[ v + diff, v' + diff , v - diff
				, v - diff, v' + diff , v' - diff
				]
	TPoly poly ->
		let pts = case poly of
			Convex [] -> []
			Convex vs -> clockwiseToTris vs
			Complex vs ix -> (vs !!) <$> ix
		in zip
			(v32v4 . (\(TP v _) -> v) <$> pts)
			(color2v3f . (\(TP _ i) -> i) <$> pts)

-- we're assuming this is a full loop, so the last value will be equal to the first one. we don't want an extra poly so we drop the final value (yes yes i know init isn't efficient; these polygons are gonna be like 8 long maximum)
clockwiseToTris :: (Eq a, Show a) => [a] -> [a]
clockwiseToTris [] = error "no points"
clockwiseToTris (o:rs) = if last rs == o
		then tris o $ Prelude.init rs
		else tris o $ rs
	where
		tris o [a,b] = [o,a,b]
		tris o (a:b:rs) = [o,a,b] ++ tris o (b:rs)
		tris _ x = error $ "bad points (" <> show (o:rs) <> ")"


v32v4 :: Num a => V3 a -> V4 a
v32v4 (V3 x y z) = V4 x y z 1

color2v3f :: ColorRGB -> V3 Float
color2v3f (RGB8 r g b) = (/ 255) . fromIntegral <$> V3 r g b

color2v4f :: ColorRGB -> V4 Float
color2v4f (RGB8 r g b) = (/ 255) . fromIntegral <$> V4 r g b 255

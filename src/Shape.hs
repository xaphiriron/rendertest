module Shape
	( RenderSpace(..)
	, pickSpace
	, pickSpaceUVs
	, renderShape
	, renderShapeUVs
	) where

import Data.Semigroup

import Linear.V2
import Linear.V3
import Linear.Metric

import Data.Color
import Data.PossibilitySpace
import Data.Turtle

data RenderSpace a b r = RenderSpace
	{ space :: PossibilitySpace a
	, toRecord :: a -> [TRecord b]
	, color :: b -> r
	}

pickSpace :: BlendableLight c => [(c, V3 Float)] -> RenderSpace a b c -> Integer -> Maybe [TRecord c]
pickSpace lights corpus i = do
	shape <- select (space corpus) i
	return $ renderShape (toRecord corpus) (color corpus) lights shape

pickSpaceUVs :: BlendableLight c => [(c, V3 Float)] -> RenderSpace a b c -> Integer -> Maybe [TRecord (c, V2 Float)]
pickSpaceUVs lights corpus i = do
	shape <- select (space corpus) i
	return $ renderShapeUVs (toRecord corpus) (color corpus) lights shape


renderShape :: BlendableLight c => (a -> [TRecord b]) -> (b -> c) -> [(c, V3 Float)] -> a -> [TRecord c]
renderShape shapeGen colorize lights shape =
	fmap (bakeLighting colorize lights) . shapeGen $ shape

renderShapeUVs :: BlendableLight c => (a -> [TRecord b]) -> (b -> c) -> [(c, V3 Float)] -> a -> [TRecord (c, V2 Float)]
renderShapeUVs shapeGen colorize lights shape =
	fmap
		(withUVs (\tp uv -> (\c -> (c, uv)) <$> tp) . bakeLighting colorize lights)
	. shapeGen
		$ shape


bakeLighting :: BlendableLight c => (a -> c) -> [(c, V3 Float)] -> TRecord a -> TRecord c
bakeLighting materialColor lights trec = extractRadiosity $ applyLights
	$ (,) 0 . materialColor <$> trec
	where
		-- applyLights :: BlendableLight c => TRecord (c, c) -> TRecord (c, c)
		applyLights = appEndo $ mconcat $ Endo . uncurry light <$> lights


light :: BlendableLight a => a -> V3 Float -> TRecord (a, a) -> TRecord (a, a)
light color angle shape = case shape of
	TVertex {} -> shape
	TLine v1 v2 -> let
			n = case p v1 - p v2 of
				V3 x 0 0 -> V3 0 0 1 `cross` (p v1 - p v2)
				_ -> V3 1 0 0 `cross` (p v1 - p v2)
			incidence = max 0 $ n `dot` angle
			radiosityf = (\(r, matColor) -> (r + (matColor ^*^ (color ^* incidence)), matColor))
		in TLine (radiosityf <$> v1) (radiosityf <$> v2)
	TPoly (Convex vs@(v1:v2:v3:_)) -> let
			n = normalize $ (p v3 - p v1) `cross` (p v2 - p v1)
			incidence = max 0 $ n `dot` angle
			radiosityf = (\(r, matColor) -> (r + (matColor ^*^ (color ^* incidence)), matColor))
		in TPoly $ Convex $ fmap radiosityf <$> vs
	TPoly (Complex vs@(v1:v2:v3:_) ix) -> let
			n = normalize $ (p v3 - p v1) `cross` (p v2 - p v1)
			incidence = max 0 $ n `dot` angle
			radiosityf = (\(r, matColor) -> (r + (matColor ^*^ (color ^* incidence)), matColor))
		in TPoly $ Complex (fmap radiosityf <$> vs) ix
	TPoly {} -> shape

-- these are not doing any HDR and will absolutely overload the values to get > 255 color components given enough lights
(^*) :: BlendableLight a => a -> Float -> a
(^*) = scale

(^*^) :: BlendableLight a => a -> a -> a
(^*^) = merge

extractRadiosity :: TRecord (a, b) -> TRecord a
extractRadiosity = fmap fst

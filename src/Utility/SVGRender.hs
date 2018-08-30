module Utility.SVGRender
	( SVGDrawData(..)
	, SVGElement(..)

	, BoundingBox(..)

	, Transform(..)

	, stroke
	, strokeWidth
	, fill
	, translate
	, toAttrs
	, asList

	, renderElement
	, groupRenderElements

	, attr
	, attrS

	, bbBox
	, bbRect
	) where

import Data.Monoid
import Data.Maybe (fromMaybe, catMaybes, listToMaybe, mapMaybe)

import Data.Ord (comparing)
import Data.List (sortBy, groupBy)
import Data.Function (on)

import Data.Color

data SVGDrawData = SVGDrawData
	{ _stroke :: Maybe ColorRGB
	, _strokewidth :: Maybe Float
	, _strokeopacity :: Maybe Float
	, _strokelinejoin :: Maybe String
	, _fill :: Maybe ColorRGB
	, _transform :: Maybe [Transform]
	}
	deriving (Eq, Ord, Show, Read)

data Transform = Translate Float Float
	deriving (Eq, Ord, Show, Read)

instance Semigroup SVGDrawData where
	SVGDrawData a b c d e f <> SVGDrawData a' b' c' d' e' f' =
		SVGDrawData (_f a a') (_f b b') (_f c c') (_f d d') (_f e e') (_m f f')
			where
				_m (Just a) (Just b) = Just (a <> b)
				_m a b = _f a b
				_f (Just a) _ = Just a
				_f Nothing b = b

instance Monoid SVGDrawData where
	mempty = SVGDrawData Nothing Nothing Nothing Nothing Nothing Nothing

stroke :: ColorRGB -> SVGDrawData
stroke s = mempty { _stroke = Just s }

strokeWidth :: Float -> SVGDrawData
strokeWidth w = mempty { _strokewidth = Just w }

fill :: ColorRGB -> SVGDrawData
fill s = mempty { _fill = Just s }

translate :: Float -> Float -> SVGDrawData
translate x y = mempty { _transform = Just $ [Translate x y] }

transforms :: [Transform] -> String
transforms = unwords . filter (not . null) . fmap (\t -> case t of
	Translate 0 0 -> []
	Translate x y -> "translate(" ++ show x ++ " " ++ show y ++ ")")


groupRenderElements :: [(SVGDrawData, SVGElement)] -> String
groupRenderElements elems = unlines $ render <$> collated
	where
		render (draw, [u]) = renderElement u (Just draw)
		render (draw, us) | all isPath us =
				let sum = Path $ unwords $ pathD `mapMaybe` us
				in renderElement sum (Just draw)
			| otherwise = unlines $
				[unwords
					[ "<g"
					, toAttrs draw
					, ">"
					]
				] <> ((\e -> renderElement e Nothing) <$> us)
				<> ["</g>"]
		collated = unitGroup elems
		isPath (Path _) = True
		isPath _ = False
		pathD (Path d) = Just d
		pathD _ = Nothing

unitGroup :: [(SVGDrawData, SVGElement)] -> [(SVGDrawData, [SVGElement])]
unitGroup = fmap (\vs -> case vs of
	[] -> error ":("
	rs@((a, _):_) -> (a, snd <$> rs)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

toAttrs :: SVGDrawData -> String
toAttrs (SVGDrawData mstroke mwidth mopacity mlinejoin mfill mtransform) =
	unwords $ catMaybes
		[ case transforms <$> mtransform of
			Just "" -> Nothing
			v -> attrS "transform" <$> v
		, attrS "stroke" . ('#' :) . asHex <$> mstroke
		, attr "stroke-width" <$> mwidth
		, attr "stroke-opacity" <$> mopacity
		, attrS "stroke-linejoin" <$> mlinejoin
		, attrS "fill" . ('#' :) . asHex <$> mfill
		]

asList :: SVGDrawData -> [(String, String)]
asList (SVGDrawData mstroke mwidth mopacity mlinejoin mfill mtransform) =
	catMaybes
		[ case transforms <$> mtransform of
			Just "" -> Nothing
			v -> (,) "transform" <$> v
		, (,) "stroke" . ('#' :) . asHex <$> mstroke
		, (,) "stroke-width" . show <$> mwidth
		, (,) "stroke-opacity" . show <$> mopacity
		, (,) "stroke-linejoin" <$> mlinejoin
		, (,) "fill" . ('#' :) . asHex <$> mfill
		]

data SVGElement
	= Circle Float Float Float
	| Line Float Float Float Float
	| Rect Float Float Float Float
	| Path String
	deriving (Eq, Ord, Show, Read)

renderElement :: SVGElement -> Maybe SVGDrawData -> String
renderElement e mdraw = unwords $ case e of
	Circle cx cy r ->
		[ "<circle"
		, attr "cx" cx
		, attr "cy" cy
		, attr "r" r
		]
	Line x1 y1 x2 y2 ->
		[ "<line"
		, attr "x1" x1
		, attr "y1" y1
		, attr "x2" x2
		, attr "y2" y2
		]
	Rect x y width height ->
		[ "<rect"
		, attr "x" x
		, attr "y" y
		, attr "width" width
		, attr "height" height
		]
	Path d ->
		[ "<path"
		, attrS "d" d
		]
	<> fromMaybe [] (pure . toAttrs <$> mdraw)
	<> [ "/>" ]


attr :: Show a => String -> a -> String
attr name val = name ++ "=\"" ++ show val ++ "\""
attrS :: String -> String -> String
attrS name val = name ++ "=\"" ++ val ++ "\""

data BoundingBox = BB Float Float Float Float
	deriving (Eq, Ord, Show, Read)

instance Semigroup BoundingBox where
	BB lx1 ly1 hx1 hy1 <> BB lx2 ly2 hx2 hy2 =
		BB (min lx1 lx2) (min ly1 ly2) (max hx1 hx2) (max hy1 hy2)

bbBox :: BoundingBox -> String
bbBox (BB lX lY hX hY) = unwords $ show <$>
	[ floor lX -- - ceiling (xSpan / 10)
	, floor lY -- - ceiling (ySpan / 10)
	, ceiling xSpan -- + ceiling (xSpan / 5) + 1
	, ceiling ySpan -- + ceiling (ySpan / 5) + 1
	]
	where
		xSpan = hX - lX
		ySpan = hY - lY

bbRect :: BoundingBox -> String ->  String
bbRect (BB lX lY hX hY) color = unwords
	[ "<rect"
	, attr "x" lX
	, attr "y" lY
	, attr "width" xSpan
	, attr "height" ySpan
	, attrS "fill" color
	, "/>"
	]
	where
		xSpan = hX - lX
		ySpan = hY - lY


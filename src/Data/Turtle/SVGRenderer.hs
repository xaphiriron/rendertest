module Data.Turtle.SVGRenderer
	( render3dViewer
	) where

import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

import Linear.V3
import Linear.Metric

import Data.Color
import Data.Turtle
import Utility.SVGRender hiding (SVGElement(Line))

data Shape3d = Line3d (V3 Float) (V3 Float) | Poly3d [V3 Float]
	deriving (Eq, Ord, Show, Read)
-- data LitShape3d = LS3D ColorRGB Shape3d
data Shape3dSVG = S3D ColorRGB SVGDrawData Shape3d
	deriving (Eq, Ord, Show, Read)

render3dViewer :: BoundingBox -> (Mode -> a -> SVGDrawData) -> ColorRGB -> [(ColorRGB, V3 Float)] -> [TRecord a] -> String
render3dViewer bb styles haze lights views = unlines $
	["<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
	, "<svg xmlns=\"http://www.w3.org/2000/svg\""
	, "    viewBox=\"" <> bbBox bb <> "\">"
	, bbRect bb (('#' :) . asHex $ haze)
	]
	<> [ "<script type=\"text/javascript\">"
	, "  function rerender3d (where, pitch, angle, shapes) {"
	, "    while (where.firstChild) {"
	, "      where.removeChild (where.firstChild);"
	, "    }"
	, "    render3d (where, pitch, angle, shapes.sort (depthCheck (pitch, angle)));"
	, "  }"
	, "  function render3d (where, pitch, angle, shapes) {"
	, "    shapes.map (function (s) {"
	, "      switch (s.type) {"
	, "        case \"l\": where.appendChild (render3dLine (pitch, angle, s.p1, s.p2, s.s));"
	, "          break;"
	, "        case \"p\": where.appendChild (render3dPoly (pitch, angle, s.ps, s.s));"
	, "          break;"
	, "        default:"
	, "          throw new Error (\"bad shape\");"
	, "      }"
	, "    });"
	, "  }"
	, "  function avg (s) {"
	, "    switch (s.type) {"
	, "      case \"l\": return average ([s.p1, s.p2]);"
	, "      case \"p\": return average (s.ps);"
	, "      default: throw new Error (\"bad shape\");"
	, "    }"
	, "  }"
	, "  function average (pts) {"
	, "    let s = pts.reduce (function (acc, p) {"
	, "      acc.n += 1;"
	, "      acc.sum.x += p.x;"
	, "      acc.sum.y += p.y;"
	, "      acc.sum.z += p.z;"
	, "      return acc;"
	, "    }, {n: 0, sum: {x:0,y:0,z:0}});"
	, "    s.sum.x = s.sum.x / s.n;"
	, "    s.sum.y = s.sum.y / s.n;"
	, "    s.sum.z = s.sum.z / s.n;"
	, "    return s.sum;"
	, "  }"
	, "  function depthCheck (pitch, angle) {"
	, "    return function (a, b) {"
	, "      var ta = pitchPoint (pitch, rotatePoint (angle, avg(a)));"
	, "      var tb = pitchPoint (pitch, rotatePoint (angle, avg(b)));"
	, "      return tb.z - ta.z;"
	, "    }"
	, "  }"
	, "  function project2dXZ (angle, pt) {"
	, "    return pt.x * Math.sin(angle) + pt.z * Math.cos(angle);"
	, "  }"
	, "  function project2dZX (angle, pt) {"
	, "    return pt.x * Math.cos(angle) - pt.z * Math.sin(angle);"
	, "  }"
	, "  function rotatePoint (angle, pt) {"
	, "    var o = {x: pt.x, y: pt.y, z: pt.z};"
	, "    o.x = pt.x * Math.sin(angle) + pt.z * Math.cos(angle);"
	, "    o.z = pt.x * Math.cos(angle) - pt.z * Math.sin(angle);"
	, "    return o;"
	, "  }"
	, "  function pitchPoint (angle, pt) {"
	, "    var o = {x: pt.x, y: pt.y, z: pt.z};"
	, "    o.y = pt.z * Math.sin(angle) + pt.y * Math.cos(angle);"
	, "    o.z = pt.z * Math.cos(angle) - pt.y * Math.sin(angle);"
	, "    return o;"
	, "  }"
	, "  function render3dLine (pitch, angle, p1, p2, styles) {"
	, "    var line = document.createElementNS(\"http://www.w3.org/2000/svg\", \"line\");"
	, "    var t1 = pitchPoint (pitch, rotatePoint (angle, p1));"
	, "    var t2 = pitchPoint (pitch, rotatePoint (angle, p2));"
	, "    line.setAttribute (\"x1\", t1.x);"
	, "    line.setAttribute (\"y1\", t1.y);"
	, "    line.setAttribute (\"x2\", t2.x);"
	, "    line.setAttribute (\"y2\", t2.y);"
	, "    styles.map (function (s) { line.setAttribute(s[0], s[1])});"
	, "    return line;"
	, "  }"
	, "  function render3dPoly (pitch, angle, pts, styles) {"
	, "    function asPath (pt) {"
	, "      return pt.x + \" \" + pt.y;"
	, "    }"
	, "    var path = document.createElementNS(\"http://www.w3.org/2000/svg\", \"path\");"
	, "    var pts2d = pts.map (function (pt) {"
	, "      return pitchPoint (pitch, rotatePoint (angle, pt));"
	, "    });"
	, "    path.setAttribute (\"d\", \"M\" + asPath (pts2d[0]) + \"L\" + pts2d.slice(1).map (asPath).reduce (function (a, b) { return a + \" \" + b;}));"
	, "    styles.map (function (s) { path.setAttribute(s[0], s[1])});"
	, "    return path;"
	, "  }"
	, "</script>"
	]
	<> [dataBox 0 views]
	<> [ "</svg>"]
	where
		dataBox i records = unlines
			[ "<script type=\"text/javascript\">"
			, unwords ["var seq" <> show i, "=", dumpData $ materialData, ";"]
			, unwords ["var seq" <> show i <> "angle = 0;"]
			, "var pitch = -30 / 180 * Math.PI;"
			, "</script>"
			, "<g id=\"" <> "seq" <> show i <> "\">"
			, "</g>"
			, "<script type=\"text/javascript\">"
			, "window.addEventListener (\"load\", function () {"
			, "  var g = document.getElementById (\"seq" <> show i <>"\");"
			, "  rerender3d (g, pitch, seq" <> show i <> "angle, seq" <> show i <> ");"
			, "  window.setInterval(function () {"
			, "    seq" <> show i <> "angle += (1.0 / 180.0) * Math.PI;"
			, "    rerender3d (g, pitch, seq" <> show i <> "angle, seq" <> show i <>");"
			, "  }, 30);"
			, "});"
			, "</script>"
			]
			where
				rawMaterialData = parseTurtleData styles records
				materialData = applyLights <$> rawMaterialData
				applyLights :: Shape3dSVG -> Shape3dSVG
				applyLights = appEndo $ mconcat $ Endo . uncurry light <$> lights

{-

		groundSquare =
			unwords
				[ "{ type: \"p\""
				, ", ps:"
				, unwords
					[ "["
					, foldr1 (\a b -> a ++ ", " ++ b) $ showV3 <$>
						[ V3 5 0 (-5)
						, V3 5 0 5
						, V3 (-5) 0 5
						, V3 (-5) 0 (-5)
						]
					, "]"
					]
				, ", s:", showAttrList $ asList $ fill "#985"
				, "}"
				]
-}

light :: ColorRGB -> V3 Float -> Shape3dSVG -> Shape3dSVG
light color angle shape = case shape of
	S3D i c (Line3d _ _) -> shape
	S3D i c poly@(Poly3d (v1:v2:v3:_)) -> let
			n = normalize $ (v3 - v1) `cross` (v2 - v1)
			incidence = max 0 $ n `dot` angle
			fcolor = _fill c
			radiosity = maybe 0 (^*^ (color ^* incidence)) fcolor
			(RGB8 r g b) ^* s = RGB8
				(floor $ fromIntegral r * s)
				(floor $ fromIntegral g * s)
				(floor $ fromIntegral b * s)
			(RGB8 r g b) ^*^ (RGB8 r' g' b') = RGB8
				(r * r' `div` 255)
				(g * g' `div` 255)
				(b * b' `div` 255)
		in S3D (i + radiosity) c poly
	S3D i c (Poly3d vs) -> shape



parseTurtleData :: (Mode -> a -> SVGDrawData) -> [TRecord a] -> [Shape3dSVG]
parseTurtleData color rs = foo `mapMaybe` rs
	where
		foo (TLine (TP v i) (TP v' i')) = Just $ S3D 0 (color Line i')
			$ Line3d v v'
		foo (TPoly vs) = case vs of
			[] -> Nothing
			(TP _ i):_ -> Just $ S3D 0 (color Polygon i)
				$ Poly3d $ (\(TP v _) -> v) <$> vs
		foo (TVertex {}) = Nothing

dumpData :: [Shape3dSVG] -> String
dumpData ss = unwords
	[ "["
	, (foldr1 (\a b -> a ++ ", " ++ b) $ dumpShape <$> ss)
	, "]"
	]
	where
		dumpShape :: Shape3dSVG -> String
		dumpShape (S3D _ c (Line3d v v')) = unwords
			[ "{ type: \"l\""
			, ", p1:", showV3 v
			, ", p2:", showV3 v'
			, ", s:", showAttrList $ asList $ c
			, "}"
			]
		dumpShape (S3D i c (Poly3d vs)) = unwords
			[ "{ type: \"p\""
			, ", ps:"
			, unwords
				[ "["
				, foldr1 (\a b -> a ++ ", " ++ b) $ showV3 <$> vs
				, "]"
				]
			, ", s:", showAttrList $ asList $ c {_fill = Just i}
			, "}"
			]
		showAttrList :: [(String, String)] -> String
		showAttrList vs = concat [ "[", intercalate "," $ fmap showAttr vs, "]"]
			where
				showAttr :: (String, String) -> String
				showAttr (a, n) = concat [ "[", show a, ",", show n, "]" ]
		showV3 (V3 x y z) = unwords
			[ "{x:" , show x
			, ",y:" , show y
			, ",z:" , show z
			, "}"
			]

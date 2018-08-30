module FRP
	( mousedown
	, mousedownM
	, onClick

	, checkbox
	, checkboxes
	) where

import Control.Arrow
import Data.Maybe

import Linear.V2

import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Control.Event.Handler

import Input.Types

mousedown :: (V2 Int -> a) -> Event FauxGLFW -> Event a
mousedown f ev = filterJust $ (\glfw -> case glfw of
	MouseDown _ pt -> Just $ f pt
	_ -> Nothing) <$> ev

mousedownM :: (V2 Int -> Maybe a) -> Event FauxGLFW -> Event a
mousedownM f ev = filterJust $ (\glfw -> case glfw of
	MouseDown _ pt -> f pt
	_ -> Nothing) <$> ev

inZeroBox :: V2 Int -> V2 Int -> Bool
inZeroBox (V2 w h) (V2 x y) = x >= 0 && x < w && y >= 0 && y < h

inBox :: V2 Int -> V2 Int -> V2 Int -> Bool
inBox bxy wh pt = inZeroBox wh . subtract bxy $ pt

-- this isn't really useful in practice V:
onClick :: V2 Int -> V2 Int -> a -> Event FauxGLFW -> Event a
onClick xy wh r ev = mousedownM ((\b -> if b then Just r else Nothing) . inZeroBox wh . subtract xy) ev


data Active a = Active a | Inactive a
	deriving (Eq, Ord, Show, Read)

data Select = Select | Deselect
	deriving (Eq, Ord, Show, Read)

checkbox :: MonadMoment m => Bool -> msg -> (a -> Bool) -> Event a -> m (Behavior (Active msg))
checkbox startActive v react listen = do
	ev <- fmap (fmap $ \b -> for b v)
		$ accumE startActive (fmap (const not) $ filterE id $ react <$> listen)
	stepper (for startActive v) ev
	where
		for b v = if b then Active v else Inactive v

checkboxes :: [Behavior (Active msg)] -> Behavior [msg]
checkboxes opts = 
	mapMaybe activeToMaybe <$> sequenceA opts
	where
		activeToMaybe :: Active a -> Maybe a
		activeToMaybe (Active a) = Just a
		activeToMaybe (Inactive _) = Nothing

-- give the checkbox values/states to here to get out a behavior that tracks the state of all checkboxes
-- this might not be as useful as possible since it's still not _that_ easy to get the state of each checkbox. arguably `[Active msg]` would be easier, or like, idk something with unique ids
checkboxes' :: MonadMoment m => [(Bool, msg)] -> Event Int -> m (Behavior [msg])
checkboxes' opts ixes = do
		let mchecks = uncurry (\ix (a, v) -> checkbox a v (\j -> j == ix) ixes) <$> keyed
		checks <- sequenceA mchecks
		return $ checkboxes checks
	where
		keyed = zip [0..] opts

radio :: MonadMoment m => Bool -> msg -> Event Select -> m (Behavior (Active msg))
radio startActive v listen =
	stepper (for startActive v) $ (\e -> case e of
			Select -> Active v
			Deselect -> Inactive v)
		<$> listen
	where
		for b v = if b then Active v else Inactive v

radios' :: MonadMoment m => [msg] -> Int -> Event Int -> m (Behavior msg)
radios' opts selected ixes = do
		let ev = filterJust $ (\i -> fmap snd . listToMaybe . drop i $ keyed) <$> ixes
		stepper (opts !! selected) ev
	where
		keyed = zip [0..] opts

{-
-- returning 1. the bounds of the flowed ui box, 2. opengl buffer data generated from the templates and 3. a reactive behavior state for the form values in the templates
flow :: MonadMoment m
	=> Texture2D os (GP.Format RGBAFloat)
	-> Fontsheet os RGBAFloat
	-> Int
	-> V2 Int
	-> [Template s raw msg]
	-> (UIBounds, ContextT GLFW.Handle os IO [UIElement os s raw msg], m (Behavior msg))

-}

-- give bounding boxes to here to get out a `FauxGLFW -> Maybe Int` function that can turn the raw event stream into a stream of which bounding box was clicked
ixHandle :: [(V2 Int, V2 Int)] -> FauxGLFW -> Maybe Int
ixHandle opts raw = (\ev -> case ev of
	MouseDown LeftClick pt -> listToMaybe
		$ (\(ix, f) -> if f pt then Just ix else Nothing) `mapMaybe` boxHit
	_ -> Nothing) raw
	where
		boxHit :: [(Int, V2 Int -> Bool)]
		boxHit = second (uncurry inBox) <$> keyed
		keyed = zip [0..] opts

ixHandle' :: [(V2 Int, V2 Int, a)] -> FauxGLFW -> Maybe a
ixHandle' opts raw = (\ev -> case ev of
	MouseDown LeftClick pt -> listToMaybe
		$ (\(ix, f, a) -> if f pt then Just a else Nothing) `mapMaybe` boxHit
	_ -> Nothing) raw
	where
		-- boxHit :: [(Int, V2 Int -> Bool, a)]
		boxHit = (\(a, (xy, wh, c)) -> (a, inBox xy wh, c)) <$> keyed
		keyed = zip [0..] opts

{-
checkbox True "plains" (const true) ...

a behavior that starts off at `startActive` and then gets applied a function (not) every time the




checkbox :: Bool -> msg -> Event a -> Event msg
checkbox initially value = do
	checked <- stepper initially ???

-}


{-
makeNetworkDescription :: AddHandler Char -> MomentIO ()
makeNetworkDescription addKeyEvent = do
    eKey <- fromAddHandler addKeyEvent

    let eOctaveChange = filterMapJust getOctaveChange eKey
    bOctave <- accumB 3 (changeOctave <$> eOctaveChange)

    let ePitch = filterMapJust (`lookup` charPitches) eKey
    bPitch <- stepper PC ePitch

    let
        bNote = Note <$> bOctave <*> bPitch
        foo = Note 0 PA

    eNoteChanged <- changes bNote
    reactimate' $ fmap (\n -> putStrLn ("Now playing " ++ show n))
<$> eNoteChanged
-}
{-# LANGUAGE PackageImports #-}

module Input.Types where

import Linear.V2

import "GPipe-GLFW" Graphics.GPipe.Context.GLFW (Key(..), KeyState(..), ModifierKeys(..), MouseButton(..), MouseButtonState(..))

data RawGLFW
	= MouseButtonEvent MouseButton MouseButtonState ModifierKeys
	| CursorPosEvent Double Double
	| KeyAction Key Int KeyState ModifierKeys
	| CharAction Char
	| WindowClose
	deriving (Eq, Ord, Show, Read)

data Button = LeftClick | RightClick
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

data FauxGLFW
	= MouseDown Button (V2 Int)
	| MouseUp Button (V2 Int)
	| MouseMove (V2 Int)
	| KeyDown Key
	| KeyUp Key
	| Typed Char
	| Close
	deriving (Eq, Ord, Show, Read)

data InputState = InputState
	{ mousePosition :: V2 Int
	}

data Symbol
	= CheckboxEmpty | CheckboxChecked
	| RadioEmpty | RadioFilled
	| UpArrow | DownArrow | LeftArrow | RightArrow
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

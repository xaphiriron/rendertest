module Data.LSystem.SignalingPlant
	( FluxPlant(..)
	, fluxPlantSpace
	, fluxPlant
	, fluxSystem

	, fluxParams
	, fluxSeed
	, fluxAdvance

	, fluxPure

	-- this should not be in this file
	, withTurtle
	, fromTurtle
	) where

import Data.LSystem
import Data.Turtle
import Data.PossibilitySpace

import Control.Monad.Identity
import Data.Tree
import Data.Maybe (isJust, fromMaybe, mapMaybe)

import Shape.PlantTypes (Plant(..))
import Data.Color
import Utility.SVGRender
import Utility.Tree

-- basipetal signal (from apexes (leaves / root tips) to base)
-- acropetal signal (from base to )
-- data SignalPlant a r = Internode | Branch | Apex
{-
data RootedPlant m r t = RootedPlant
	{ roots :: FullContextLSystem m r
	, trunk :: FullContextLSystem m t
	, signalToRoots :: t -> r -> Maybe r -- given the current values of trunk and roots, potentially generate a new roots base value
	, signalToTrunk :: r -> t -> Maybe t -- given the current values of roots and trunk, potentially generate a new trunk base value
	}
-}
{-
    ∧ in a math operation - the exponentiation operator ∧

Symbols that control turtle orientation in space (Figure 6a)
+ (θ) Turn left by angle θ around the ~U axis.
- (θ) Turn right by angle θ around the ~U axis.
& (θ) Pitch down by angle θ around the ~L axis.
^ (θ) Pitch up by angle θ around the ~L axis.
/ (θ) Roll left by angle θ around the ~H axis.
\ (θ) Roll right by angle θ around the ~H axis.
| Turn 180 around the ~U axis. This is equivalent to +(180) or -(180).

    = in a forward context (likely 'ignore until matching ...')




ignore: + - /
-}

fluxPlantSpace :: PossibilitySpace (Plant (Float, Maybe (Either Float Int)) Identity (TurtleSymbol (Float, Maybe (Either Float Int)) FluxPlant))
fluxPlantSpace = (\param -> let
		system = FCLSystem isTurtlePush isTurtlePop isTurtleAction
			$ \left pred right -> pure $ fluxAdvance param left pred right
	in FCPlantIndexed
		{ postProcess = fluxPostProcess
		, fclsystem = system
		, seed = A <$> fluxSeed
		, zero = (_σ0 param, Just (Right 1))
		, index = \(_, c) -> case c of
			Nothing -> fill (RGB8 0xe0 0xe0 0xe0)
			Just (Left _) -> fill (RGB8 0xff 0x40 0x80)
			Just (Right _) -> fill (RGB8 0x40 0x60 0xff)
		})
		<$> paramSpace
	where
		


fluxPlant :: Plant (Float, Maybe (Either Float Int)) Identity (TurtleSymbol (Float, Maybe (Either Float Int)) FluxPlant)
fluxPlant = FCPlantIndexed
	{ postProcess = fluxPostProcess
	, fclsystem = fluxSystem
	, seed = A <$> fluxSeed
	, zero = (17, Just (Right 1))
	, index = \(_, c) -> case c of
		Nothing -> fill (RGB8 0xe0 0xe0 0xe0)
		Just (Left _) -> fill (RGB8 0xff 0x40 0x80)
		Just (Right _) -> fill (RGB8 0x40 0x60 0xff)
	}

fluxPostProcess :: TurtleSymbol (Float, Maybe (Either Float Int)) FluxPlant -> [TurtleSymbol (Float, Maybe (Either Float Int)) FluxPlant]
fluxPostProcess c = case c of
	A (Internode _ msg flux apices) ->
		[ c
		, TA $ Color $ (,) flux $ case msg of
			Nothing -> Nothing
			Just Acropetal -> Just $ Left flux
			Just Basipetal -> Just $ Right $ round apices
		, TA $ Advance 3
		]
	x -> [x]

fluxSystem :: FullContextLSystem Identity (TurtleSymbol a FluxPlant)
fluxSystem = FCLSystem isTurtlePush isTurtlePop isTurtleAction $ \left pred right -> pure $ fluxAdvance flux3dParams left pred right

data FluxPlant = N Float | Internode Segment (Maybe Message) Float Float | Apex
	deriving (Eq, Ord, Show, Read)

data Segment = Base | Straight | Lateral
	deriving (Eq, Ord, Enum, Bounded, Show, Read)
data Message = Acropetal | Basipetal
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

{-
#define α1  10    /* branching angle - straight segment */
#define α2  32    /* branching angle - lateral segment */
#define σ0  17    /* initial flux */
#define η    0.87 /* controls input flux changes */
#define λ    0.7  /* flux distribution factor */
#define υth  5.0  /* threshold flux for branching */
-}
data FluxParams = FluxParams
	{ _α1 :: Float -- branching angle, in degrees - straight segment
	, _α2 :: Float -- branching angle, in degrees - lateral segment
	, _β :: Float -- roll angle, in degrees. was always 180 in initial system
	, _σ0 :: Float -- initial flux
	, _η :: Float  -- controls input flux changes
	, _λ :: Float  -- flux distribution factor
	, _vth :: Float -- threshhold flux for branching
	}

fluxParams :: FluxParams
fluxParams = FluxParams
	{ _α1 = 10
	, _α2 = 32
	, _β = 180
	, _σ0 = 17
	, _η = 0.87
	, _λ = 0.7
	, _vth = 5.0
	}

flux3dParams :: FluxParams
flux3dParams = FluxParams
	{ _α1 = 10
	, _α2 = 32
	, _β = 153
	, _σ0 = 17
	, _η = 0.87
	, _λ = 0.7
	, _vth = 5.0
	}

paramSpace :: PossibilitySpace FluxParams
paramSpace = FluxParams
	<$> (fromIntegral <$> rangeNum (5, 20))
	<*> (fromIntegral <$> rangeNum (15, 45))
	<*> (fromIntegral <$> rangeNum (90, 270))
	<*> (fromIntegral <$> rangeNum (10, 25))
	<*> ((/ 100) . fromIntegral <$> rangeNum (50, 99))
	<*> ((/ 100) . fromIntegral <$> rangeNum (50, 99))
	<*> (fromIntegral <$> rangeNum (3, 8))


fluxSeed :: [FluxPlant]
fluxSeed = [N 1, Internode Base (Just Basipetal) 0 1, Apex]


{-
ω : N(1) I(0,2,0,1) A
p1:           N(k) < I(b,m,υ,c)                                   : b == 0 && m == 2             -> I(b, 1, σ0*2 ∧ (k - 1) * (η ^ k), c)
p2:                  N(k)       > I(b,m,υ,c)                      : b == 0 && m == 2             -> N(k + 1)
p3: I(bl,ml,υl,cl) < I(b,m,υ,c)                                   : ml == 1 && b == 1            -> I(b, ml, υl - υl * (1 - λ) * ((cl - c)/c), c)
p4: I(bl,ml,υl,cl) < I(b,m,υ,c)                                   : ml == 1 && b == 2            -> I(b, ml, υl * (1 - λ) * (c/(cl - c)), c)
p5:     I(b,m,υ,c) < A                                            : m == 1 && υ > υth            -> /(180) [-(α2) I(2, 2, υ * (1 - λ), 1)A] +(α1) I(1, 2, υ * λ, 1) A
p6:                  I(b,m,υ,c) > A                               : m == 1 && υ <= υth           -> I(b, 2, υ, c)
p7:                  I(b,m,υ,c) > [I(b2,m2,υ2,c2)=]I(b1,m1,υ1,c1) : m == 0 && m1 == 2 && m2 == 2 -> I(b, 2, υ, c1 + c2)
p8:                  I(b,m,υ,c)                                   : m == 1                       -> I(b, 0, υ, c)
p9: I(bl,ml,υl,cl) < I(b,m,υ,c)                                   : ml == 2 && m == 2            -> I(b, 0, υ, c)
-}
fluxAdvance :: FluxParams -> [TurtleSymbol a FluxPlant] -> TurtleSymbol a FluxPlant -> Tree [TurtleSymbol a FluxPlant] -> [TurtleSymbol a FluxPlant]
fluxAdvance p = withTurtle (fluxPure p)

withTurtle :: ([a] -> a -> Tree [a] -> [TurtleSymbol t a]) -> [TurtleSymbol t a] -> TurtleSymbol t a -> Tree [TurtleSymbol t a] -> [TurtleSymbol t a]
withTurtle f left pred right = case pred of
	TA _ -> pure $ pred
	A apred -> f
		(fromTurtle `mapMaybe` left)
		apred
		(removeEmpties $ fmap (mapMaybe fromTurtle) $ right)

fromTurtle :: TurtleSymbol t a -> Maybe a
fromTurtle (TA _) = Nothing
fromTurtle (A a) = Just a

fluxPure :: FluxParams -> [FluxPlant] -> FluxPlant -> Tree [FluxPlant] -> [TurtleSymbol a FluxPlant]
fluxPure p left pred right = case (left, pred, right) of
	((N k):_, Internode Base (Just Basipetal) v c, _)
		-> pure $ A $ Internode Base (Just Acropetal) (σ0 * 2 ** (k - 1) * (η ** k)) c -- p1
	(_, N k, Node ((Internode Base (Just Basipetal) v c):_) _)
		-> pure $ A $ N (k + 1) -- p2
	((Internode _ (Just Acropetal) vl cl):_, Internode Straight _ v c,_)
		-> pure $ A $ Internode Straight (Just Acropetal) (vl - vl * (1 - λ) * ((cl - c)/c)) c -- p3
	((Internode _ (Just Acropetal) vl cl):_, Internode Lateral _ v c, _)
		-> pure $ A $ Internode Lateral (Just Acropetal) (vl * (1 - λ) * (c/(cl - c))) c -- p4
	((Internode _ (Just Acropetal) v cf):_, Apex, _) | v > vth
		->
			[ TA $ Roll (deg2rad β)
			, TA Push, TA $ Turn (deg2rad (-α2))
			, A $ Internode Lateral (Just Basipetal) (v * (1 - λ)) 1, A $ Apex
			, TA $ Pop, TA $ Turn (deg2rad α1)
			, A $ Internode Straight (Just Basipetal) (v * λ) 1, A $ Apex
			] -- p5
	(_, Internode b (Just Acropetal) v c, Node (Apex:_) _) | v <= vth
		-> pure $ A $ Internode b (Just Basipetal) v c -- p6
	(_, Internode b Nothing v c, Node [] ((Node ((Internode b2 (Just Basipetal) _ c2):_) _) : (Node ((Internode b1 (Just Basipetal) _ c1):_) _) : _))
		-> pure $ A $ Internode b (Just Basipetal) v (c1 + c2) -- p7
	(_, Internode b (Just Acropetal) v c, _)
		-> pure $ A $ Internode b Nothing v c -- p8
	((Internode _ (Just Basipetal) _ _):_, Internode b (Just Basipetal) v c, _)
		-> pure $ A $ Internode b Nothing v c -- p9
	(left, pred, right) -> pure $ A pred -- default rule


	where
		α1 = _α1 p
		α2 = _α2 p
		β = _β p
		σ0 = _σ0 p
		η = _η p
		λ = _λ p
		vth = _vth p
		deg2rad d = d / 180 * pi


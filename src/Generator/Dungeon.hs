{-# LANGUAGE LambdaCase #-}

module Generator.Dungeon where

import Prelude hiding (flip)

import Data.Graph.Inductive

import Data.GraphGrammar

import Utility.Rand

import Linear.V3

{-

basic line:

[ mkGraph [(0,2),(1,1)] [(0,1,(2,1))]
, mkGraph [(0,1),(1,2)] [(0,1,(1,2))]
]

let foo = emap (\e -> case e of {'w' -> ED Impassible 1 e; _ -> ED Open 1 e}) $ nmap (\n -> ND 0 [] 1 n) $ mkGraph [(0, 'p'), (1, 'p'), (2, 'p'), (3, 'p')] [(0, 1, 'c'), (1, 2, 'b'), (2, 3, 'c'), (0, 3, 'w')] :: Gr (GenNode Char) (GenEdge Char)

let barMatch = mkGraph [(0, \(ND _ _ _ n) -> n == 'p'), (1, \(ND _ _ _ n) -> n == 'p')] [(0, 1, \(ED _ _ e) -> e == 'b')]
let barExp = mkGraph [(0, Copy 0), (1, Copy 1), (2, Static $ ND 0 [] 0 'o')] [(0, 2, Copy (0, 1)), (2, 1, Copy (0, 1))]

let bar = Exp barMatch barExp

runRand (mkStdGen 0) $ isos barMatch foo



let foo = emap (\e -> case e of {'w' -> ED Impassible 1 e; _ -> ED Open 1 e}) $ nmap (\n -> ND 0 [] 1 n) $ mkGraph [(0, 'p'), (1, 'p'), (2, 'p'), (3, 'p')] [(0, 1, 'c'), (1, 2, 'b'), (2, 3, 'c'), (0, 3, 'w')] :: Gr (GenNode Char) (GenEdge Char)

let barMatch = mkGraph [(0, \(ND _ _ _ n) -> n == 'p'), (1, \(ND _ _ _ n) -> n == 'p')] [(0, 1, \(ED _ _ e) -> e == 'b')]
let barExp = mkGraph [(0, Copy 0), (1, Copy 1), (2, Static $ ND 0 [] 0 'o')] [(0, 2, Copy (0, 1)), (2, 1, Copy (0, 1))]

let bar = Exp barMatch barExp

runRand (mkStdGen 0) $ isos barMatch foo




let foo = emap (\e -> case e of {'w' -> ED Impassible 1 e; _ -> ED Open 1 e}) $ nmap (\n -> ND 0 [] 1 n) $ mkGraph [(0, 'p'), (1, 'p'), (2, 'p'), (3, 'p')] [(0, 1, 'c'), (1, 2, 'b'), (2, 3, 'c'), (0, 3, 'w')] :: Gr (GenNode Char) (GenEdge Char)

let barMatch = mkGraph [(0, \(ND _ _ _ n) -> n == 'p'), (1, \(ND _ _ _ n) -> n == 'p') ,(2, \(ND _ _ _ n) -> n == 'p')] [(0, 1, \(ED _ _ e) -> e == 'b') ,(2, 0, \(ED _ _ e) -> e == 'c') ]
let barExp = mkGraph [(0, Copy 0), (1, Copy 1), (2, Static $ ND 0 [] 0 'o')] [(0, 2, Copy (0, 1)), (2, 1, Copy (0, 1))]

let bar = Exp barMatch barExp

runRand (mkStdGen 0) $ isos barMatch foo




runRand (mkStdGen 0) $ isos barMatch foo

-}




data GenEdge a = ED Lock Difficulty a
	deriving (Eq, Ord, Show)

-- distinguish 'open' from 'default connection' (for postprocessing); add in 'locked by switch' or 'unlocked by switch' (i.e., open when switch is on vs. closed when switch is on)
data Lock
	= Default -- presumed Open but not necessarily definitively open
	| Open -- can be traversed in either direction
	| LockedWith Key -- blocked in both directions until key Key is used
	| LockedFromSide Side -- blocked from one direction, but can be opened from the other direction to allow both-way traversal
	| OneWay Side -- permanently only traversable from one direction (e.g., a drop)
	| UnlockedBySwitch Switch
	| LockedBySwitch Switch
	| Impassible -- a window/chasm connection, used only for structuring the graph
	deriving (Eq, Ord, Show)

data Side = Towards | Against -- since edges are technically directed, Side is just saying if the locked/open direction for (a, b, data) is a -> b (Towards) or b -> a (Against)
	deriving (Eq, Ord, Show)

data Key = Key Char -- :V
	deriving (Eq, Ord, Show)
data Teleporter = Teleporter Char
	deriving (Eq, Ord, Show)
data Switch = Switch Char
	deriving (Eq, Ord, Show)

type Difficulty = Int

-- and hardcoded node attributes would be...

data GenNode p a
	= ND
		{ _p :: p
		, _provides :: [Provides]
		, _difficulty :: Difficulty
		, _a :: a
		}
	deriving (Eq, Ord, Show)

instance Functor (GenNode p) where
	fmap f (ND p ls d a) = ND p ls d (f a)

-- add in: teleporter, switch
data Provides
	= HasKey Key
	| HasSwitch Switch
	| HasTeleporter Teleporter
	deriving (Eq, Ord, Show)


data DungeonState = DS
	{ zonesPlaced :: Int
	, keysUsed :: String
	}

nullState :: DungeonState
nullState = DS 0 []



-- note that this loses key/lock/directional data
strip :: DynGraph gr => gr (GenNode p n) (GenEdge e) -> gr n e
strip = nemap (\(ND _ _ _ n) -> n) (\(ED _ _ e) -> e)



flipSide :: Flip Side
flipSide = Flip $ \case
	Towards -> Against
	Against -> Towards

flipLock :: Flip Lock
flipLock = Flip $ \case
	LockedFromSide s -> LockedFromSide $ flip flipSide s
	OneWay s -> OneWay $ flip flipSide s
	l -> l

flipGenEdge :: Flip a -> Flip (GenEdge a)
flipGenEdge rev = Flip $ \(ED l d a) -> ED (flip flipLock l) d (flip rev a)

flipId :: Flip a
flipId = Flip $ id


embedGenNode :: Embedder p (GenNode p n)
embedGenNode = Embedder $ \pos nd -> nd { _p = pos }




dummyEmbed :: (RandomGen g, Graph gr) => p -> Embedding g p gr n e
dummyEmbed zero gr _ = Just $ pure $ \i -> if uncurry within (nodeRange gr) i
	then zero
	else error "dummyEmbed: node outside graph range"

within :: Ord n => n -> n -> n -> Bool
within lo hi v = lo <= v && v <= hi

stopAtSize :: Graph gr => Int -> Stop gr n e
stopAtSize max gr = noNodes gr >= max

dummyGenerator :: (Graph gr, RandomGen g) => GraphGrammar g (V3 Float) () gr (GenNode (V3 Float) n) (GenEdge e)
dummyGenerator = GG empty () [] (dummyEmbed $ V3 0 0 0) (stopAtSize 0) (Just $ flipGenEdge flipId) embedGenNode


{-
canyonBase =
	emap (\e -> case e of {'w' -> ED Impassible 1 e; _ -> ED Open 1 e})
	$ nmap (\n -> ND 0 [] 1 n)
	$ mkGraph
		[(0, 'p'), (1, 'p'), (2, 'p'), (3, 'p')]
		[(0, 1, 'c'), (1, 2, 'b'), (2, 3, 'c'), (0, 3, 'w')] :: Gr (GenNode Char) (GenEdge Char)

barMatch = mkGraph
	[(0, \(ND _ _ _ n) -> n == 'p'), (1, \(ND _ _ _ n) -> n == 'p')]
	[(0, 1, \(ED _ _ e) -> e == 'b')]
barExp = mkGraph
	[(0, Copy 0), (1, Copy 1), (2, Static $ ND 0 [] 0 'o')]
	[(0, 2, Copy (0, 1)), (2, 1, Copy (0, 1))]

forkCanyon = Exp barMatch barExp

canyon expansions:

-- elongate cliff edge
a-C-b     a-C-e-C-b
|   |     |   |   |
*   * ->- *   W   *
|   |     |   |   |
c-C-d     c-C-f-C-d

-- fork canyon
a-*-b     a-*-b-C-e
|   |     |   |   |
C   C ->- C   W   B
|   |     |   |   |
c-*-d     c-*-d-C-f

-- make bridge island?
a-B-b -> a-B-i-B-c


-}

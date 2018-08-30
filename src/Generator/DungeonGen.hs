module Generator.DungeonGen
	( dungeonGenerator
	, dungeonGeneratorLoop
	, Zone(..)

	, totalSize
	) where

import Data.GraphGrammar
import Data.Graph.Inductive

import Data.Monoid
import Data.Maybe
import Data.List

import Generator.Dungeon

import Utility.Rand

import Shape.Hex (Shape, containedInShape)

data Zone = Zone
	{ index :: Int
	, treasure :: Bool
	}
	deriving (Eq, Ord, Show, Read)

baseDungeon :: RandomGen g => (Int -> p) -> Rand g (Gr (GenNode p Zone) (GenEdge e))
baseDungeon starter = return $
	mkGraph
		[ (0, ND (starter 0) [] 0
			$ Zone 0 False)
		]
		[]
{-
reverseTestBase :: (Int -> p) -> Gr (GenNode p Zone) (GenEdge ())
reverseTestBase starter =
	mkGraph
		[ (0, ND (starter 0) [] 0 $ Zone 0 False)
		, (1, ND (starter 1) [] 0 $ Zone 0 False)
		]
		[ (0, 1, ED (OneWay Against) 0 ())
		]

oneWayExpand :: Graph gr => (Zone -> Rand g p) -> DungeonState -> Rand g (Expansion gr (GenNode p Zone) (GenEdge ()), DungeonState)
oneWayExpand fp acc = do
	p <- fp $ Zone 0 False
	return $
		(Exp
			(mkGraph
				[ (0, \_ -> True)
				, (1, \_ -> True)
				]
				[ (0, 1, \(ED l _ _) -> l == OneWay Against)
				])
			(mkGraph
				[ (0, Copy 0)
				, (1, Copy 1)
				, (2, Static $ ND p [] 0 (Zone 0 False))
				]
				[ (0, 2, Static $ ED Open 0 ())
				, (1, 2, Static $ ED Open 0 ())
				])
		, acc
		)
-}

{-
thinking about how to create a linear dungeon

a very simple but extremely slow way would be to start with an 'exit' label, and have the state track the 'deepest' room placed, and have an 'advance' expansion that fails unless the room's label matches the state's deepest room. the advance expansion would make a new room off the deepest room, move the exit label, and set the state room value to the new room. there could be branching advancements -- lock the new door, and place the key in an offshoot room -- or misc side treasure rooms branching from non-exit rooms.

(that gets into wanting to have different expansions available for different nodes, which... is currently doable, right? by matching on specific parts of the node. so really we wouldn't need a state count, we'd just need to match only on the node with the exit label. the issue _there_ is that there's no length guarantee; the exit advancement could only trigger once, leading to a very short map.

	(i mean that could be a part of every expansion; "if this room has the exit in it, remove it and place it in one of the other rooms")

alternately, uhhhh. each time an expansion runs a room variable could increment, and when it reaches x then that room can no longer be expanded? or that could change which expansions are available?)

the main issue is, i don't think there's a way to... force branches to be meaningful? well no, there is, it's that each time you let a branch get to x deep you terminate its growth, place an unused key there, and store the key id in the global accum. and then the next time you grow the main branch you use the latest stored key(s) to lock the expansion path. but that means needing to grow the main branch enough to use all the locked keys, which isn't guaranteed to happen at alllll.

being able to switch the expansion process would be nice, too -- currently it's random, but it might be good to add in random-with-ordering, or even full l-system "run an evolution rule on every node in the graph", but that would mean figuring out some way to encode that a la l-system expansion rules (prefix, postfix, etc aren't really meaningful in a graph context. presumably you could declare a matching subgraph but then only allow editing of node 0 in the graph).

random-with-ordering would be a good way to force keys to be used, since you could put the to-be-keyed room at the highest priority if there are any keys queued. that still wouldn't handle the case where the 'main path' room is boxed in though, but... idk hopefully that wouldn't happen much VV:
-}

dungeonGeneratorLoop :: (RandomGen g) =>
	   (Zone -> Rand g Shape)
	-> (Int -> Shape)
	-> Embedding g Shape Gr (GenNode Shape Zone) (GenEdge ())
	-> (Gr (GenNode Shape Zone) (GenEdge ()) -> Bool)
	-> Rand g (GraphGrammar g Shape DungeonState Gr (GenNode Shape Zone) (GenEdge ()))
dungeonGeneratorLoop fp = dungeonGeneratorRaw
	[ oneWayCircuitPair fp
	, lockedCircuit fp
	, keyDoor 2 fp
	, stretchZone fp
	, unlockableTeleporter fp
	, expandEdge fp -- turns an open edge into an open passage through a new room
	, biDirectionalLockedCircuit fp -- turns an open edge into an unlocking loop traversable from either side
	]

dungeonGenerator :: (RandomGen g) =>
	   (Zone -> Rand g Shape)
	-> (Int -> Shape)
	-> Embedding g Shape Gr (GenNode Shape Zone) (GenEdge ())
	-> (Gr (GenNode Shape Zone) (GenEdge ()) -> Bool)
	-> Rand g (GraphGrammar g Shape DungeonState Gr (GenNode Shape Zone) (GenEdge ()))
dungeonGenerator fp = dungeonGeneratorRaw
	[ keyDoor 1 fp -- add a locked door
	, keyDoor 2 fp -- add two locked doors using the same key
	, stretchZone fp -- split a zone in two, also splitting its keys
	, oneWayCircuitSingle fp -- turn a single node into a looping one-way circuit
	, oneWayCircuitPair fp -- turn an open edge into a looping one-way circuit
	, lockedCircuit fp -- convert an open edge into a locked circuit with a key in a side room off one path. the open edge remains (which is important b/c if that open edge was a chokepoint it could make the map unsolvable)
	]

dungeonGeneratorRaw :: (RandomGen g) =>
	[DungeonState -> Rand g (Expansion Gr (GenNode Shape Zone) (GenEdge ()), DungeonState)]
	-> (Int -> Shape)
	-> Embedding g Shape Gr (GenNode Shape Zone) (GenEdge ())
	-> (Gr (GenNode Shape Zone) (GenEdge ()) -> Bool)
	-> Rand g (GraphGrammar g Shape DungeonState Gr (GenNode Shape Zone) (GenEdge ()))
dungeonGeneratorRaw expansions starter embedding quitFunc = do
	landscape <- baseDungeon starter
	return $ GG
		landscape
		nullState
		expansions
		embedding
		quitFunc
		(Just $ flipGenEdge flipId)
		embedGenNode

shapeSize :: Shape -> Int
shapeSize = length . containedInShape

freeSpaces :: GenNode Shape Zone -> Int
freeSpaces (ND s items _ (Zone _ t)) = shapeSize s - (length items + (if t then 1 else 0))

totalSize :: Int -> Gr (GenNode Shape Zone) (GenEdge ()) -> Bool
totalSize usedTiles gr = (usedTiles <=) . sum $ shapeSize . _p . snd <$> labNodes gr

{- new expansions:

* find a one-way unlockable passage and lock it with a key in an adjacent room on the openable side (this can't lock open passages b/c there's no way to tell if it's a chokepoint or on which side, but a one-way unlockable passage already has that information)
* add two offshoot rooms with keys, and then two locked doors to two new rooms, which then both connect to a third room. basically adds choice (you need one or the other key to progress)
* add two offshoot rooms with keys, and then a locked door to a new room with another locked door to a third room. adds multi-linearity (do each wing in either order, but both must be done)
* add switches which unlock locked doors
	* ...and then find a room with a switch-locked door and add another switch-locked door

have specific zones generate, and have them limited to certain room shapes/sizes as well as max connections allowed
-}

keyDoor :: Graph gr => Int -> (Zone -> Rand g Shape) -> DungeonState -> Rand g (Expansion gr (GenNode Shape Zone) (GenEdge ()), DungeonState)
keyDoor uses fp (DS z ks) = do
	let k = case ks of
		[] -> 'a'
		h:_ -> succ h
	-- p <- fp $ Zone (z+1)
	ps <- sequence $ fmap fp $ (Zone <$> [1..uses] <*> pure True)
	let newNodes = zipWith (\p i -> (i + 2, Static $ ND p [] 0 (Zone (z + i) True))) ps [1..uses]
	let newEdges = (\i -> (0, i + 2, Static $ ED (LockedWith $ Key k) 0 ())) <$> [1..uses]
	return $
		(Exp
			(mkGraph
				[ (0, \_ -> True)
				, (1, \(n, _) -> freeSpaces n > 0)
				]
				[ (0, 1, const True) -- should this require an open edge? node 1 will always get the key, and node 0 will always get the locked doors.
				])
			(mkGraph
				([ (0, Copy 0)
				, (1, Mutate 1 $ fmap rmTreasure . (addKey $ Key k))
				] <> newNodes)
				([ (0, 1, Copy (0, 1))
				] <> newEdges))
		, DS (z+uses) (k:ks)
		)

addKey :: Key -> GenNode p Zone -> GenNode p Zone
addKey k (ND p ks d a) = ND p (HasKey k : ks) d a

addTeleporter :: Teleporter -> GenNode p Zone -> GenNode p Zone
addTeleporter t (ND p ks d a) = ND p (HasTeleporter t : ks) d a

rmTreasure :: Zone -> Zone
rmTreasure (Zone i _) = Zone i False

addTreasure :: Zone -> Zone
addTreasure (Zone i _) = Zone i True

unlockableTeleporter :: Graph gr => (Zone -> Rand g Shape) -> DungeonState -> Rand g (Expansion gr (GenNode Shape Zone) (GenEdge ()), DungeonState)
unlockableTeleporter fp (DS z ks) = do
	let t = case ks of
		[] -> 'a'
		h:_ -> succ h
	p <- fp $ Zone (z+1) False
	return $
		( Exp
			(mkGraph
				[ (0, \(n, _) -> freeSpaces n > 0)
				, (1, const True)
				]
				[])
			(mkGraph
				[ (0, Mutate 0 $ addTeleporter (Teleporter t))
				, (1, Copy 1)
				, (2, Static $ addTeleporter (Teleporter t) $ ND p [] 0 $ Zone (z+1) False)
				]
				[ (1, 2, Static $ ED (LockedFromSide Against) 0 ())
				])
		, DS (z+1) (t:ks)
		)

stretchZone :: (Graph gr, RandomGen g) => (Zone -> Rand g Shape) -> DungeonState -> Rand g (Expansion gr (GenNode Shape Zone) (GenEdge ()), DungeonState)
stretchZone fp acc = do
	r <- liftRand $ random
	p' <- fp $ Zone 9 False -- at this point in the code i don't have access to the actual zone data, so...
	return $
		( Exp
			(mkGraph
				[ (0, \_ -> True)
				]
				[])
			-- we limit the items taken to make sure there's room in the destination room for all the points
			(mkGraph
				[ (0, Mutate 0 $ \n@(ND p ps d a) ->
					let takenIndices = min (freeSpaces $ n { _p = p', _provides = []}) (r * genericLength ps)
					in ND p (drop takenIndices ps) d (rmTreasure a))
				, (1, Mutate 0 $ \n@(ND _ ps d a) ->
					let takenIndices = min (freeSpaces $ n { _p = p', _provides = []}) (r * genericLength ps)
					in ND p' (take takenIndices ps) d a)
				]
				[ (0, 1, Static $ ED Open 0 ())
				])
		, acc
		)

{-
breakList :: Float -> [a] -> ([a],[a])
breakList f as = (take fi as, drop fi as)
	where
		fi = floor $ f * genericLength as
-}

-- instead of expanding a single node, this should maybe split an Open edge into a looping circuit.
-- or, have an alternate form where it splits an Open edge into a circuit where the return edge is locked and the key is in the room after the one-way drop
oneWayCircuitSingle :: (Graph gr, RandomGen g) => (Zone -> Rand g p) -> DungeonState -> Rand g (Expansion gr (GenNode p Zone) (GenEdge ()), DungeonState)
oneWayCircuitSingle fp acc = do
	p <- fp $ Zone 9 False
	q <- fp $ Zone 9 False
	pLock <- fromMaybe (error "bad list")
		<$> randomFromList [OneWay Towards, Open, Open]
	qLock <- fromMaybe (error "bad list")
		<$> randomFromList [OneWay Against, LockedFromSide Against, LockedFromSide Against]
	-- make ONE of the rooms have treasure, but not both or none
	t <- liftRand $ randomR (0, 1) :: RandomGen g => Rand g Int
	let pTreasure = if t == 0 then rmTreasure else addTreasure
	let qTreasure = if t == 0 then addTreasure else rmTreasure
	return $
		(Exp
			(mkGraph
				[ (0, \_ -> True)
				]
				[])
			(mkGraph
				[ (0, Mutate 0 $ fmap rmTreasure)
				, (1, Mutate 0 $ \(ND _ _ _ a) -> ND p [] 0 $ pTreasure a)
				, (2, Mutate 0 $ \(ND _ _ _ a) -> ND q [] 0 $ qTreasure a)
				]
				[ (0, 1, Static $ ED (OneWay Towards) 0 ())
				, (1, 2, Static $ ED pLock 0 ())
				, (0, 2, Static $ ED qLock 0 ())
				])
		, acc
		)

oneWayCircuitPair :: (Graph gr, RandomGen g) => (Zone -> Rand g p) -> DungeonState -> Rand g (Expansion gr (GenNode p Zone) (GenEdge ()), DungeonState)
oneWayCircuitPair fp acc = do
	q <- fp $ Zone 9 True
	pLock <- fromMaybe (error "bad list")
		<$> randomFromList [OneWay Towards, Open, Open]
	qLock <- fromMaybe (error "bad list")
		<$> randomFromList [OneWay Against, LockedFromSide Against, LockedFromSide Against]
	return $
		(Exp
			(mkGraph
				[ (0, const True)
				, (1, const True)
				]
				[ (0, 1, \(ED l _ _) -> l == Open)
				])
			(mkGraph
				[ (0, Mutate 0 $ fmap rmTreasure)
				, (1, Copy 1)
				, (2, Mutate 0 $ \(ND _ _ _ a) -> ND q [] 0 $ addTreasure a)
				]
				[ (0, 1, Static $ ED (OneWay Towards) 0 ())
				, (1, 2, Static $ ED pLock 0 ())
				, (0, 2, Static $ ED qLock 0 ())
				])
		, acc
		)

expandEdge :: (Graph gr, RandomGen g) => (Zone -> Rand g p) -> DungeonState -> Rand g (Expansion gr (GenNode p Zone) (GenEdge ()), DungeonState)
expandEdge fp acc = do
	q <- fp $ Zone 0 True
	return $
		(Exp
			(mkGraph
				[ (0, const True)
				, (1, const True)
				]
				[ (0, 1, \(ED l _ _) -> l == Open)
				])
			(mkGraph
				[ (0, Mutate 0 $ fmap rmTreasure)
				, (1, Copy 1)
				, (2, Mutate 0 $ \(ND _ _ _ a) -> ND q [] 0 $ addTreasure a)
				]
				[ (0, 2, Static $ ED Open 0 ())
				, (2, 1, Static $ ED Open 0 ())
				])
		, acc
		)

biDirectionalLockedCircuit :: (Graph gr, RandomGen g) => (Zone -> Rand g p) -> DungeonState -> Rand g (Expansion gr (GenNode p Zone) (GenEdge ()), DungeonState)
biDirectionalLockedCircuit fp (DS z ks) = do
	p <- fp $ Zone (z+1) False
	q <- fp $ Zone (z+2) False
	earlyP <- fromMaybe (error "bad list")
		<$> randomFromList [True, False]
	earlyQ <- fromMaybe (error "bad list")
		<$> randomFromList [True, False]
	return $ (Exp
		(mkGraph
			[ (0, const True)
			, (1, const True)
			]
			[ (0, 1, \(ED l _ _) -> l == Open)
			])
		(mkGraph
			[ (0, Mutate 0 $ fmap rmTreasure)
			, (1, Mutate 1 $ fmap rmTreasure)
			, (2, Static $ ND p [] 0 $ Zone (z+1) False)
			, (3, Static $ ND q [] 0 $ Zone (z+2) False)
			]
			[ (0, 2, Static $ ED (if earlyP then LockedFromSide Towards else Open) 0 ())
			, (2, 1, Static $ ED (if earlyP then Open else LockedFromSide Towards) 0 ())
			, (0, 3, Static $ ED (if earlyQ then Open else LockedFromSide Against) 0 ())
			, (3, 1, Static $ ED (if earlyQ then LockedFromSide Against else Open) 0 ())
			])
		, DS (z+2) ks
		)

lockedCircuit :: (Graph gr, RandomGen g) => (Zone -> Rand g p) -> DungeonState -> Rand g (Expansion gr (GenNode p Zone) (GenEdge ()), DungeonState)
lockedCircuit fp (DS z ks) = do
	p <- fp $ Zone (z+1) False
	q <- fp $ Zone (z+2) False
	r <- fp $ Zone (z+3) False
	let k = case ks of
		[] -> 'a'
		h:_ -> succ h
	pLock <- fromMaybe (error "bad list")
		<$> randomFromList [OneWay Towards, Open, Open, Open]
	qLock <- fromMaybe (error "bad list")
		<$> randomFromList [OneWay Against, LockedFromSide Against, Open, Open, Open, Open]
	return $ (Exp
		(mkGraph
			[ (0, const True)
			, (1, const True)
			]
			[ (0, 1, \(ED l _ _) -> l == Open)
			])
		(mkGraph
			[ (0, Copy 0)
			, (1, Copy 1)
			, (2, Static $ ND p [] 0 $ Zone (z+1) False)
			, (3, Static $ ND r [HasKey $ Key k] 0 $ Zone (z+2) False)
			, (4, Static $ ND q [] 0 $ Zone (z+3) False)
			]
			[ (0, 1, Copy (0, 1))
			, (0, 2, Static $ ED pLock 0 ())
			, (2, 3, Static $ ED Open 0 ())
			, (1, 4, Static $ ED qLock 0 ())
			, (2, 4, Static $ ED (LockedWith $ Key k) 0 ())
			])
		, DS (z+3) (k:ks)
		)

{- -- can't be used due to how there's no way to communicate the picked key from the match to the expansion side of the Exp value
branchLockedDoor :: (Graph gr, RandomGen g) => (Zone -> Rand g p) -> DungeonState -> Rand g (Expansion gr (GenNode p Zone) (GenEdge ()), DungeonState)
branchLockedDoor fp (DS z ks) = do
	p <- fp $ Zone (z+1) True
	return $ (Exp
			(mkGraph
				[ (0, const True)
				, (1, const True)
				, (2, const True)
				]
				[ (0, 1, \(ED l _ _) -> case l of
					LockedWith (Key k) -> True
					_ -> False)
				, (0, 2, \(ED l _ _) -> l == Open)
				])
			(mkGraph
				[ (0, Copy 0)
				, (1, Copy 1)
				, (2, Copy 2)
				, (3, Static $ ND p [] 0 $ Zone (z+1) True)
				]
				[ (0, 1, Copy (0, 1))
				, (0, 2, Copy (0, 2))
				, (2, 3, Static $ ED (LockedWith $ Key k) 0 ())
				])
		, DS (z+1) ks
		)
-}

{-
Zone 0

and expansion rules like

-- keyDoor, above
Zone n -> Zone n (containing A) -|A- Zone (+1)

Zone n           Zone n -|A- Zone (+1)
Zone m        -> Zone m (containing A)
(where m > n)
(i.e., a match on two disconnected nodes, where one is later in the progression than the other, and placing the connection in the earlier zone while the progression item is in the later one)

w/ some bonus stuff like...
-- stretchZone, above
Zone n -> Zone n --- Zone m (just expand an area out without adding any lock/keys)

-- oneWayCircuit, above
                  v one way
Zone n -> Zone n ->>- Zone n
             \----<|----/
                  ^ unlockable

Zone n -|x- Zone m
(where 'x' is any item already placed)

things to add that might be difficult to add:
1. a key used in several nearby places
	(we can place a locked door for any key already used, but there's no guarantee of closeness.)
2. keys specifically placed behind nearby locked doors
	(we could have zones contain the set of keys required to get there, so then if you want to add key x to a room, you do that and then the room behind the x door would have whatever keys it takes to get to 1. the key room and 2. the locked door, plus x.)
	(alternately we could have 'pending' keys, where we put down a key behind _every_ locked door, and then just use the next locked door as the last key)
3. a room that can be unlocked from different directions by different keys, creating a circuit when both/all are unlocked

things that are probably _really_ difficult to add:
1. 'small keys' that are one-use and any one can be used to unlock any 'small key' door
	1a. that will never let you run out while there are still locked small key doors
		(i think this might require either 1. entirely trivial generators or 2. actual routing checks)




powerups / common keys
	expansion 1: place a powerup and an adjoining node that's locked w/ it
	expansion 2: pick a random powerup already placed, and a random node, and add an adjoining node that's locked w/ it

--branchLockedDoor, above, but can't be used yet
diversify locked doors
	pick a node pair with a locked edge
	from one of those nodes (the one with the lower zone?), add (or pick) an adjoining open edge to another node, and then add a locked edge sharing the key as the other locked edge, to a treasure room

	              /<- A ->\
	             /    |    \
	A --- B ->  C     ^     D
	             \    -    /
	              \>- B -</

	have a path that transforms into a fork, and the rejoin point also features a one-way unlockable route backwards to the initial room. have one fork be more difficult than the other.

start calculating difficulty 
	* zones start at difficulty 0, and every node is +1 from the base node
	* difficulty would ultimately determine enemy count

place enemies
place bosses
	placed either 1. right before the finish marker or 2. right before the key that leads, directly, to the finish

	minibosses could be placed somewhere along a necessary path
place start/finish markers

-}
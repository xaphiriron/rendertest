module Data.GraphGrammar
	( GraphGrammar(..)
	, generate
	, Selection(..)
	, Expansion(..)
	, Embedding
	, Stop
	, Flip(..)
	, Embedder(..)
	, GenError(..)

	-- internal functions that might be useful to use on their own
	, isos
	, expansions
	, allMatches
	) where

import Prelude hiding (flip)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Merge.Lazy (merge, traverseMissing, traverseMaybeMissing, zipWithAMatched)
import Data.Maybe
import Data.Monoid
import Data.Tuple
import Data.List

import Control.Arrow ((&&&))

import Data.Graph.Inductive

import Utility.Rand (Rand(..), RandomGen, liftRand)
import Utility.Shuffle (shuffle)
import Utility.Misc (mapAccumLM)
import Utility.Tuple (liftTuple, liftFst)
import Data.Graph.Inductive.Utility (lnmap, lemap, alterNodeLabels)

-- import Debug.Trace

{-
ASSUMPTIONS THIS MAKES ABOUT THE GRAPH STRUCTURE OF THE INPUTS:

* only one edge for both (n,m) and (m,n) between any two nodes
* edges are bidirectional if you pass in a `Flip` function, otherwise they're not. this still might break if both (m,n) and (n,m) exist
* an expansion graph will always have at least as many nodes as the match graph (if it has fewer, that could disconnect the graph)

i don't think this will break things, but there might be weirdness:

* the starting graph is always fully connected, and each expansion graph is also fully connected

-}

{- THINGS TODO:

	* provide adjacent node context to the match code? so that you can say "only add this node foo if this node isn't adjacent to a node of type bar"
	* weight expansions somehow? or make them available or not based on some measure of overall graph state? "no there are too many nodes of this type so stop generating them"
		* maybe also provide a function that orders nodes, and transpose the subgraph selection -- instead of picking a random expansion and matching on every node in the graph (before moving on to another expansion), pick the first node of the ordering and try every expansion on it (before moving on to the second node)
	* let expansions have randomness? something like changing the second val to...
		(gr (Rand g (Selection (GenNode n) Int)) (Rand g (Selection (GenEdge e) (Int, Int))))
		i guess??


* make sure this actually translates cutting edges correctly, b/c i have some circumstantial evidence that it doesn't
* there was a bug where edges that were matched backwards were not properly cut/replaced. that should be fixed now; i don't know if there are any other edge-related bugs

* i don't think bidirectional edges are tested in the `Mutate` case
-}

-- in an expansion result, new edges/nodes are one of these: a manually-declared data value, a copy of an existing value, or a mutation of an existing value
data Selection v i = Static v | Copy i | Mutate i (v -> v)

-- b/c of the v->v function this isn't a functor in v also
instance Functor (Selection v) where
	fmap _ (Static v) = Static v
	fmap f (Copy i) = Copy $ f i
	fmap f (Mutate i vf) = Mutate (f i) vf

data Expansion gr n e
	= Exp
		(gr ((n, [e]) -> Bool) (e -> Bool))
		(gr (Selection n Int) (Selection e (Int, Int)))
	| ExpContext
		(gr ((n, [e]) -> Bool) (e -> Bool))
		(gr n e -> Maybe (gr n e))
{-
data Expansion gr n e = Exp
	(gr ((n, [e]) -> Bool) (e -> Bool))
	(gr n e -> gr n e) -- the argument to this list is the subgraph, with its nodes renumbered from 0 and with all its external edges cut. the output will be reintegrated into the map, with new node ids and external edges restored.
-}

type Embedding g p gr n e = gr n e -> [Int] -> Maybe (Rand g (Int -> p))
type Stop gr n e = gr n e -> Bool

newtype Flip a = Flip { flip :: a -> a }

newtype Embedder p n = Embedder { embed :: p -> n -> n }

data GraphGrammar g p acc gr n e = GG
	{ start :: gr n e -- the zero graph which is used as the initial base
	, expansionContext :: acc
	-- , expansionDeck :: [Rand g (Expansion gr n e)]
	, expansionDeck :: [acc -> Rand g (Expansion gr n e, acc)]
	, embedding :: Embedding g p gr n e
	, stop :: Stop gr n e
	, flipEdge :: Maybe (Flip e)
	, embedder :: Embedder p n
	}

data GenError = NoExpansions | ExhaustedExpansions
	deriving (Eq, Ord, Show, Read)

generate :: (DynGraph gr, RandomGen g, Show (gr n e), Show n, Show e)
	=> GraphGrammar g p acc gr n e -> Rand g (Either GenError (gr n e))
generate gen = case expansionDeck gen of
	[] -> return $ Left NoExpansions
	_ -> f (expansionContext gen) (Right $ start gen)
	where
		-- advance :: gr (GenNode n) (GenEdge e) -> Rand g (Either GenError (gr (GenNode n) (GenEdge e)))
		advance acc cur = do
			rs <- expansions gen acc cur
			case filter (\(gr', new, _) -> isJust $ embedding gen gr' new) rs of
				[] -> return $ Left ExhaustedExpansions
				(next, new, acc'):_ -> case embedding gen next new of
					Just x -> do
						rGraph <- reembed (embedder gen) next <$> x
						return $ Right (rGraph, acc')
					Nothing -> error "ran out of embeddings, maybe(?)"
		-- f :: Either GenError (gr (GenNode n) (GenEdge e)) -> Rand g (Either GenError (gr (GenNode n) (GenEdge e)))
		f acc mcur = case mcur of
			Left err -> return $ Left err
			Right cur -> if stop gen cur
				then return $ Right cur
				else do
					enext <- advance acc cur
					case enext of
						Right (next, acc') -> f acc' (Right next)
						_ -> error "ran out of expansions, maybe(?)"

reembed :: DynGraph gr => Embedder p n -> gr n e -> (Int -> p) -> gr n e
reembed re gr f = lnmap (\(i, n) -> embed re (f i) n) gr

-- all possible expansions of the current graph, in a lazy list
expansions :: (DynGraph gr, RandomGen g, Show (gr n e), Show n, Show e)
	=> GraphGrammar g p acc gr n e -> acc -> gr n e -> Rand g [(gr n e, [Node], acc)]
expansions gen acc gr = do
	deck <- sequence =<< liftRand (shuffle (($ acc) <$> expansionDeck gen))
	--genDeck <- sequence deck
	concat <$> (\(exp, acc') -> case exp of
		Exp match expansion -> do
			isoGraph <- isos (flipEdge gen) match gr
			return $ ((\(gr', news) -> (gr', news, acc'))
					. integrate (flipEdge gen) expansion gr)
				<$> isoGraph
		ExpContext match expfunc -> do
			isoGraphs <- isos (flipEdge gen) match gr
			return $ (\iso -> let
					flipFailError = error "matched backwards edge when that should be impossible"
					mExp =
						expfunc
							$ asSubgraph
								(fromMaybe flipFailError
									$ flipEdge gen)
								gr iso
					in case mExp of
						Nothing -> Nothing
						Just exp -> let
								mergedExpansion =
									canonizeEdges (fromMaybe flipFailError $ flipEdge gen)
									. rightJoin iso
										$ exp
							in Just $ (\(gr', news) -> (gr', news, acc'))
								$ reintegrate mergedExpansion gr)
				`mapMaybe` isoGraphs
		) `mapM` deck

rightJoinMap :: Ord k => Map k a -> Map k b -> Map k (Maybe a, b)
rightJoinMap = merge
	(traverseMaybeMissing $ \k l -> pure Nothing)
	(traverseMissing $ \k r -> pure (Nothing, r))
	(zipWithAMatched $ \k l r -> pure (Just l, r))

rightJoin :: Graph gr => gr a b -> gr c d -> gr (Maybe a, c) (Maybe b, d)
rightJoin a b = mkGraph
	(Map.toList nodes)
	((\((n, m), e) -> (n, m, e)) <$> Map.toList edges)
	where
		nodes = rightJoinMap aNodeSet bNodeSet
		aNodeSet = Map.fromList $ labNodes a
		bNodeSet = Map.fromList $ labNodes b
		edges = rightJoinMap aEdgeSet bEdgeSet
		aEdgeSet = Map.fromList $ (\(n, m, e) -> ((n, m), e)) <$> labEdges a
		bEdgeSet = Map.fromList $ (\(n, m, e) -> ((n, m), e)) <$> labEdges b

-- subgraph contains a bunch of nodes/edges to overwrite, also some new nodes/edges to add. also potentially some edges to cut? in that if an edge between labeled nodes in the subgraph doesn't exist, then the corresponding edge should be cut in `gr`. return the altered graph plus a list of new nodes

canonizeEdges :: DynGraph gr => Flip e -> gr n (Maybe (Node, Node, Direction), e) -> gr n (Maybe (Node, Node), e)
canonizeEdges (Flip flip) = emap (uncurry canonizeEdge)
	where
		canonizeEdge mb e = case mb of
			Nothing -> (Nothing, e)
			Just (n, m, Forwards) -> (Just (n, m), e)
			Just (n, m, Backwards) -> (Just (m, n), flip e)

reintegrate :: DynGraph gr => gr (Maybe Node, n) (Maybe (Node, Node), e) -> gr n e -> (gr n e, [Node])
reintegrate subgraph gr =
	(insEdges newEdges
	. insNodes newNodeIx
	. updateEdges
	. updateNodes
		$ gr, newNodesList)
	where
		updateNodes = lnmap $ \(n, v) -> fromMaybe v $ lookup n subgraphNodes
		updateEdges = lemap $ \(n, m, e) -> fromMaybe e $ lookup (n, m) subgraphEdges
		newEdges = fmap (\(n, m, (_, e)) -> (reix n, reix m, e))
			. filter (isNothing . fst . (\(_, _, c) -> c))
				$ labEdges subgraph
		unmatchedNodes = fmap (\(n, (_, v)) -> (n, v))
			. filter (isNothing . fst . snd)
				$ labNodes subgraph
		newNodesList = newNodes (length unmatchedNodes) gr
		newNodeIx = zipWith (\(_, v) newIx -> (newIx, v)) unmatchedNodes newNodesList
		newIxes = zipWith (\(unmatched, _) newIx -> (unmatched, newIx)) unmatchedNodes newNodesList
		reix i = case lookup i $ labNodes subgraph of
			Nothing -> error "bad index"
			Just (Just n, _) -> n
			Just (Nothing, _) -> case lookup i newIxes of
				Just j -> j
				Nothing -> error "failed index lookup"
		subgraphEdges = (liftFst . \(_, _, c) -> c) `mapMaybe` labEdges subgraph
		subgraphNodes = (liftFst . snd) `mapMaybe` labNodes subgraph

{-
tho ideally we'd merge those two iso graphs so it's like
	gr
		(Maybe Node, n)
		(Maybe (Node, Node, Direction), e)
with a Just value signifying an identifier for a node to update/replace, and a Nothing signifying a new node/edge

			isoGraph
-}

asSubgraph :: DynGraph gr => Flip e -> gr n e -> gr Node (Node, Node, Direction) -> gr n e
asSubgraph (Flip flip) base indices = emap setEdge . nmap setNode $ indices
	where
		edgeMap = Map.fromList $ (\(n, m, e) -> ((n, m), e)) <$> labEdges base
		-- setEdge :: (Node, Node, Direction) -> e
		setEdge (n, m, dir) = case dir of
			Forwards -> fromMaybe (error "bad subgraph") $ Map.lookup (n, m) edgeMap
			Backwards -> flip $ fromMaybe (error "bad subgraph") $ Map.lookup (m, n) edgeMap

		nodeMap = Map.fromList $ labNodes base
		-- setNode :: Node -> n
		setNode n = fromMaybe (error "bad subgraph") $ Map.lookup n nodeMap


data Direction = Backwards | Forwards
	deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- return the list of subgraph isomorphisms: the graph will be isomorphic to `gi`, and contain as values the corresponding node (or node pair for edges) in the `gj` graph
-- additionally, matched edges will have a Direction value, saying if they were matched forwards or backwards
-- this considers all edges to be bidirectional, and uses `flip` to reverse edge label data when matching on an edge backwards, if applicable
isos :: (DynGraph gr, RandomGen g)
	=> Maybe (Flip e) -> gr ((n, [e]) -> Bool) (e -> Bool) -> gr n e -> Rand g [gr Node (Node, Node, Direction)]
isos biedge subgraphMatch gj = do
	shuffledNodes <- liftRand $ shuffle $ nodes gj
	return $ do
		(fixed, _) <- mapAccumLM (buildIso shuffledNodes) [] (nodes subgraphMatch)
		return
			$ lemap ((\(i, j) -> case (,) <$> lookup i fixed <*> lookup j fixed of
					-- we never really store if a given edge is matched backwards or forwards, since we only fix nodes, so here we have to re-derive whether this edge is backwards.
					-- the constant assumption is that any given nodes will only ever have one edge connecting them. we verify this here, kind of.
					Just (fixedI, fixedJ) -> case
						( length $ filter (\(n, m) -> n == fixedI && m == fixedJ) $ edges gj
						, length $ filter (\(n, m) -> m == fixedI && n == fixedJ) $ edges gj
						) of
							(1, 0) -> (fixedI, fixedJ, Forwards)
							(0, 1) -> (fixedI, fixedJ, Backwards)
							_ -> error "multiple edges between a single node! unable to resolve edge forward/backward connection"
					Nothing -> error "missing fixed node")
				. (\(a, b, _) -> (a, b)))
			. lnmap (fromMaybe (error "missing fixed node")
				. (\i -> lookup i fixed)
				. fst)
				$ subgraphMatch
	where
		nmatch i j = case (lab subgraphMatch i, match j gj) of
			(Just f, (Just (in_, _, n, out), _)) -> f (n, fst <$> in_ <> out)
			_ -> False

		-- buildIso :: [Node] -> [(Node, Node)] -> Node -> ([(Node, Node)], ([(Node, Node)], (Node, [(Node, Node)]))) -- this isn't right
		buildIso shuffledNodes fixed ni = let
				isValidFixed templateNode matchNode =
					maybe True (matchNode ==) (lookup templateNode fixed)
					&& maybe True (templateNode ==) (lookup matchNode $ swap <$> fixed)
				eis = lsuc subgraphMatch ni
			in do
			m <- [m | m <- shuffledNodes, nmatch ni m, isValidFixed ni m]
			matchingEdges <- allMatches
				(\(iTo, f) (njFrom, njTo, e) -> if njFrom == m -- don't need to (re)check valid; we did that when getting `m`
					then isValidFixed iTo njTo && f e
					else case biedge of
						Nothing -> False
						Just biedge' -> isValidFixed iTo njFrom && f (flip biedge' e)
				) eis $ out gj m <> inn gj m
			-- these are a list of nodes that have become fixed via edge checks
			let fixedEdgePairs =
				(\(iTo, (jFrom, jTo)) -> if jFrom == m -- again we need to check for reversed edges
					then [(ni, jFrom), (iTo, jTo)]
					else [(ni, jTo), (iTo, jFrom)])
				=<< zipWith (\ei (jf, jt, _) -> (fst ei, (jf, jt))) eis matchingEdges
			let newFixed = (\(ni, nj) -> case lookup ni fixed of
				Just nj' -> if nj == nj'
					then Nothing -- we already know about this fix
					else error $ unwords
						[ "double match:"
						, show ni
						, "was fixed for both"
						, show nj
						, "and"
						, show nj'
						]
				Nothing -> Just (ni, nj)) `mapMaybe` ((ni, m) : fixedEdgePairs)
			-- the snd of this tuple is a list in the form [(Node, [(Node, Node)])], which should correspond to the output of `nodes subgraphMatch`, containing first a node match on j, and then a list of successive edge matches on corresponding `subgraphMatch` node, some of which might be backwards (i.e., an edge like (m, n) rather than (n, m))
			return (newFixed <> fixed, (m, (\(a, b, _) -> (a, b)) <$> matchingEdges) )

allMatches :: (a -> b -> Bool) -> [a] -> [b] -> [[b]]
allMatches match = allMatches' []
	where
		allMatches' _ [] _ = [[]] -- ran out of requirements; success
		allMatches' _ _ [] = [] -- ran out of potential matches; failure
		allMatches' rems (next:reqs) (opt:opts) =
			if match next opt
				{- if we match, flow down both the path made by using the match and the path made by not. the former is pretty obvious but the latter is for cases like
					> allMatches (\req opt -> opt >= req) [3,7] [9,3]
				the 9 can match both 7 and 3, but the 3 can only match 3. so if we only go down the path where we match 3 with 9 we can't get to the solution; we have to continue on without either using or forgetting about the other option values
				-}
				then ((opt :) <$> allMatches' [] reqs (rems <> opts)) -- consume matching opt, reset rems, advance
					<> allMatches' (opt:rems) (next:reqs) opts -- ignore matching pattern, advance
				else allMatches' (opt:rems) (next:reqs) opts -- store failed opt, advance

-- FIXME: about half the errors in this ought to be unrepresentable, because they're things like 'get a node id from a graph, and then try to get the node label for that id from the same graph' -- it would be nice for that to not exist, so that the only errors we have to write are the ones that can actually happen
-- expansions with backward edges are matched correctly, but the backward edges aren't removed as they should be
integrate :: (DynGraph gr, Show (gr n e), Show n, Show e)
	=> Maybe (Flip e)
	-> gr (Selection n Node) (Selection e (Node, Node))
	-> gr n e
	-> gr Node (Node, Node, Direction)
	-> (gr n e, [Node])
integrate biEdge expansion base iso =
	(   insEdges newEdges -- (trace ("newEdges: " <> show newEdges) newEdges) -- add in w/e new edges turned up in `expansion`
		. delEdges matchedEdges -- (trace ("matchedEdges: " <> show matchedEdges) matchedEdges) -- delete all edges matched in `iso`
		. alterNodeLabels alteredNodes -- (trace ("alteredNodes: " <> show alteredNodes) alteredNodes) -- alter any existing nodes that were touched in `iso`, using values from `expansion`
		. insNodes expansionLNodes -- (trace ("expansionLNodes: " <> show expansionLNodes) expansionLNodes) -- add in w/e new nodes turned up in `expansion`
			$ base -- (trace ("base: " <> show base) base)
	, newBaseNodes
	)
	where
		newExpansionNodes = nodes expansion \\ nodes iso
		newBaseNodes = newNodes (length newExpansionNodes) base

		getMatchingNode :: Node -> Maybe Node
		getMatchingNode i = case lab iso i of
			Just l -> Just l
			Nothing -> case lookup i $ zip newExpansionNodes newBaseNodes of
				Just l -> Just l
				Nothing -> Nothing

		alteredNodes = (\(isoNode, baseNode) -> case lab expansion isoNode of
				Nothing -> error "missing node in expansion (expansion must be at least as large as the subgraph it matches, and must be numbered the same way)"
				Just sel -> (baseNode, nodeResolve base . remapSelectionNode $ sel))
			<$> labNodes iso -- :: [(Node, GenNode n)]

		-- the node label (`l`) is a node id for the corresponding node in `base`; the label in the edge is the pair of node ids corresponding to a matching `base` node (which might be backwards)
		matchedEdges = fmap (\(n, m, dir) -> case dir of
				Forwards -> (n, m)
				Backwards -> (m, n)
			) $ (\(_, _, _, out) -> fst <$> out)
			=<< ufold (:) [] iso :: [Edge]

		expansionLNodes = fromMaybe (error "missing nodes in expansion?!")
			$ (liftTuple . (getMatchingNode &&& fmap (nodeResolve base . remapSelectionNode) . lab expansion))
				`mapM` newExpansionNodes --  :: [(Node, GenNode n)]

		newEdges = (\(n, m, sel) -> case (,) <$> getMatchingNode n <*> getMatchingNode m of
				Just (n', m') -> (n', m', edgeResolve biEdge base $ remapSelectionEdge sel)
				Nothing -> error "unable to match node for some reason i can't figure out right now")
			<$> labEdges expansion -- :: [(Node, Node, Selection ...)]

		remapSelectionNode :: Selection n Node -> Selection n Node
		remapSelectionNode = fmap (\i -> fromMaybe (error $ "nonexistent iso node referenced in expansion (" <> show i <> ")") $ lab iso i)

		remapSelectionEdge :: Selection e (Node, Node) -> Selection e (Node, Node)
		remapSelectionEdge = fmap (\(n, m) -> case (,) <$> lab iso n <*> lab iso m of
			Just (n', m') -> (n', m')
			Nothing -> error $ "nonexistent iso node(s?) referenced in expansion (" <> show n <> "," <> show m <> ")")

nodeResolve :: Graph gr => gr n e -> Selection n Node -> n
nodeResolve _ (Static n) = n
nodeResolve gr (Copy i) = fromMaybe (error "nodeResolve: given index outside of graph")
	$ lab gr i
nodeResolve gr (Mutate i f) = fromMaybe (error "nodeResolve: given index outside of graph")
	$ f <$> lab gr i

edgeResolve :: Graph gr => Maybe (Flip e) -> gr n e -> Selection e (Node, Node) -> e
edgeResolve _ _ (Static e) = e
-- i can't think of a better way to resolve matching the expansion of backwards edges, so we're just doing it like this.
edgeResolve biEdge gr (Copy (n, m)) = case filter (\(m', _) -> m == m') $ lsuc gr n of
	[] -> case filter (\(m', _) -> m == m') $ lpre gr n of
		[] -> error $ "edgeResolve(Copy): no such edge " <> show (n, m) <> " in graph"
		(_, e):_ -> case biEdge of
			Just biEdge' -> flip biEdge' e
			Nothing -> error $ "edgeResolve(Copy): no such edge " <> show (n, m) <> " in graph (and no bi-directional edges)"
	(_, e):_ -> e
-- TODO: HEY CHECK REVERSE EDGES HERE ALSO
edgeResolve _ gr (Mutate (n, m) f) = case filter (\(m', _) -> m == m') $ lsuc gr n of
	[] -> error $ "edgeResolve(Mutate): no such edge " <> show (n, m) <> " in graph"
	(_, e):_ -> f e

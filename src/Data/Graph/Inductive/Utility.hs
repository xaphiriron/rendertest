module Data.Graph.Inductive.Utility
	( lnmap
	, lemap
	, labEdge
	, alterNodeLabels

	, gmapM
	) where

import Data.Graph.Inductive

lnmap :: DynGraph gr => (LNode a -> b) -> gr a c -> gr b c
lnmap f = let nodeUpdate (in_, n, a, out) = (in_, n, f (n, a), out) in gmap nodeUpdate

-- i'm not 100% this handles edge node ordering right
lemap :: DynGraph gr => (LEdge b -> c) -> gr a b -> gr a c
lemap f =
	let nodeUpdate (in_, n, a, out) =
		( (\(e, m) -> (f (m, n, e), m)) <$> in_
		, n, a
		, (\(e, m) -> (f (n, m, e), m)) <$> out)
	in gmap nodeUpdate

labEdge :: Graph gr => gr a b -> Node -> Node -> [LEdge b]
labEdge gr n m = filter (\(_, m', _) -> m == m') $ out gr n

alterNodeLabels :: DynGraph gr => [(Node, a)] -> gr a b -> gr a b
alterNodeLabels modified = gmap $ \(in_, n, l, out) -> case lookup n modified of
	Nothing -> (in_, n, l, out)
	Just l' -> (in_, n, l', out)

gmapM :: (DynGraph gr, Monad m) => (Context a b -> m (Context c d)) -> gr a b -> m (gr c d)
gmapM f = ufold (\c mgr -> do
	c' <- f c
	gr <- mgr
	return $ c' & gr) $ return empty

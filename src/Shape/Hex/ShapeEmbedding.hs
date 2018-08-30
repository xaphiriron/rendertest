module Shape.Hex.ShapeEmbedding
	( shapeEmbedding
	) where

import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

import Shape.Hex
import Data.Graph.Inductive
import Data.GraphGrammar
import Data.PossibilitySpace

import Utility.Rand

shapeEmbedding :: (RandomGen g, Graph gr) => (Shape -> Bool) -> (n -> Shape) -> Embedder Shape n -> Embedding g Shape gr n e
shapeEmbedding externalConstraints fromN _ gr new =
	if count permutations == 0
		then Nothing
		else Just $ do
			nflookup <- roll permutations
			return $ \i -> if i `elem` new
				then nflookup i
				else fromMaybe (error "bad graph") $ fromN <$> lookup i (labNodes gr)
	where
		permutations :: PossibilitySpace (Node -> Shape)
		permutations = verify new
		verify :: [Node] -> PossibilitySpace (Node -> Shape)
		verify new = snd <$> foldM enumPositions ([], \_ -> error "missing node") new
			where
				enumPositions :: ([Node], (Node -> Shape)) -> Node -> PossibilitySpace ([Node], Node -> Shape)
				enumPositions (newFixed, nfLookup) n =
						(,)
							<$> pure (n : newFixed)
							<*> shapeFuncs
					where
						shapeFuncs :: PossibilitySpace (Node -> Shape)
						shapeFuncs = from $ (\s i -> if i == n then s else nfLookup i) <$> shapes
						-- shapes :: [Shape]
						-- shapes = newShapeParam <$> acceptableCenters
						shapes :: [Shape]
						shapes = if null usedAdjacent
							then []
							else Set.toList
								. (Set.filter $ (\s -> (externalConstraints s &&) $ Set.null $ Set.fromList (containedInShape s) `Set.intersection` usedHexes))
								. foldr1 Set.intersection
									$
										(\matchShape -> case (matchShape, newShape) of
											-- do the simple adjacency check for hex + hex shapes
											(HSize c r, HSize _ newR) -> Set.map newShapeParam $ Set.fromList
												$ ring c (r + newR + 1)
											-- if the matched shape isn't a hex, but the new shape is, do the complex adjacency check
											(_, HSize _ _) -> Set.map newShapeParam . adjacentToShape . (`constructPhantomShape` matchShape) $ newShape
											-- if the new shape isn't a hex, do the complex adjacency check + rotate the new shape to see if it will fit
											_ -> Set.unions $
												(\rotShape -> Set.map (\c -> moveShapeTo c rotShape) . adjacentToShape . (`constructPhantomShape` matchShape) $ rotShape)
												<$> (rotateShape newShape <$> [0..5])
											)
										<$> usedAdjacent
						usedAdjacent :: [Shape]
						usedAdjacent =
							((fromN . fromMaybe (error "bad graph") . (`lookup` (labNodes gr)))
								<$> filter (\i' -> not $ i' `elem` new) (neighbors gr n))
							<> (nfLookup
								<$> filter (\i' -> i' `elem` newFixed) (neighbors gr n))
						usedHexes :: Set Hex
						usedHexes =
							(Set.unions $ fmap (Set.fromList . containedInShape . fromN . snd) . filter (\(i', _) -> not $ i' `elem` new) $ labNodes gr)
							<> (Set.unions $ Set.fromList . containedInShape . nfLookup <$> newFixed)
						newShapeParam :: Hex -> Shape
						newShapeParam h = moveShapeTo h newShape
						newShape :: Shape
						newShape = fromMaybe (error "bad graph") $ fromN <$> lookup n (labNodes gr)



{-
		verify :: [Node] -> PossibilitySpace (Node -> Shape)
		verify new = snd <$> foldr enumPositions (pure ([], \_ -> error "missing node")) new
			where
				enumPositions :: Node -> PossibilitySpace ([Node], (Node -> Shape)) -> PossibilitySpace ([Node], Node -> Shape)
				enumPositions n en = do
					(newFixed, nfLookup) <- en
					let usedHexes =
						(Set.unions $ fmap (Set.fromList . containedInShape . fromN . snd) . filter (\(i', _) -> not $ i' `elem` new) $ labNodes gr)
						<> (Set.unions $ Set.fromList . containedInShape . nfLookup <$> newFixed)
					let usedAdjacent =
						((fromN . fromMaybe (error "bad graph") . (`lookup` (labNodes gr)))
							<$> filter (\i' -> not $ i' `elem` new) (neighbors gr n))
						<> (nfLookup
							<$> filter (\i' -> i' `elem` newFixed) (neighbors gr n))
					let acceptableCenters = if null usedAdjacent
						then []
						else Set.toList . (Set.filter $ (\c -> Set.null $ Set.fromList (containedInShape $ HSize c newRadius) `Set.intersection` usedHexes)) . foldr1 Set.intersection $
							(\s -> case s of
								HSize c r -> Set.fromList $ ring c (r + newRadius + 1)
								_ -> error "unexpectedly-shaped shape") <$> usedAdjacent
					let shapes = (\c -> HSize c newRadius) <$> acceptableCenters
					let shapeFuncs = from $ (\s i -> if i == n then s else nfLookup i) <$> shapes
					if null shapes
						then mzero
						else (,) <$> pure (n : newFixed) <*> shapeFuncs
					where
						newRadius :: Int
						newRadius = case newShape of
							HSize _ r -> r
							_ -> error "unexpectedly-shaped shape"
						newShape :: Shape
						newShape = fromMaybe (error "bad graph") $ fromN <$> lookup n (labNodes gr)
-}

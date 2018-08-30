module Data.PossibilitySpace
  ( PossibilitySpace (count)
  , select
  , roll
  , draw
  , oneCard
--  , handOf
  , withdraw
  , enumerate

  , decompose
  , construct

  , mutate
  , breed

  , rangeEnum
  , rangeNum
  , bounds
  , from

  , permutations
  , permutationsA
  , addOrNot
  , addOrNotA

  , rollsOfN
  , rollsOfNRange
  , drawsOfN
  , drawsOfNRange

  , uniqueDrawsOfN -- this is not a fast bind so don't use it for large sets

  , deckOf

  , mapWithIndex
  , numPow

  , fixedBind -- DO NOT USE UNLESS YOU REALLY KNOW WHAT YOU'RE DOIN
  , fixedJoin -- DITTO
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.List (mapAccumR, foldl')

import Utility.Rand

import Debug.Trace

data PossibilitySpace a = PossibilitySpace
  { count :: Integer
  , segments :: [Integer]
  , selector :: Integer -> a
  }

{-
data Segments = Seg
	{ baseAxes :: [Integer]
	, derivedAxes :: Int
	, derivedAxisMap :: Int -> (Int, [Integer])
	}

this won't track for `decomp` or `mutate`, which have further problems, but it will make `dependentBind` faster (i.e., not slower) for repeating the same axis

baseAxes is the same as `segments` currently
derivedAxes is the length of segments plus the number of times we've done a dependent bind (basically)
derivedAxisMap is a function that returns bound segments
-}

{- 
this is where things begin to be half-implemented.
* the current Alternative instance ignores segment data
* the current Monad instance is (probably unavoidably) extremely slow

* Monoid and MonadPlus are both implemented in terms of Alternative, so they
  inherit the Alternative issue

so, my specific problems i'm asking for help / insight with, in order of
importance:

* an Alternative instance requires some more complex math that i haven't
  figured out, and would probably involve dropping storing segments as
  [Integer] in favor of some more detailed nested structure -- since [Integer]
  only works because <*> multiplies possibility spaces together, a la cartesian
  joins, meaning that every space constructed only with <*> will be a
  n-dimensional rectangular prism. since Alternative could join
  arbitrarily-sized spaces, there's no longer an even "axis length" value to
  store. something like
    data Segment
      = Combination [(Segment, Integer)]
      | Choice [(Segment, Integer)]
  seems like it could work, maybe, i just haven't been able to work out
  precisely how to implement <*> and <|> (plus `deconstruct`) for all
  permutations of values
* there's a `functionOf :: PossibilitySpace a -> PossibilitySpace b -> PossibilitySpace (a -> b)`
  function. (return the deck of all functions taking all inputs in `a` and
  mapping them to any input in `b`. exhaustively constructs the entire function
  space.) it doesn't work. specifically, it depends on `unselect`, which i'm
  pretty sure cannot be written. however, since i do have `numPow` working, it
  seems possible that there's some other angle to approach this problem that
  doesn't run into the same issue. maybe? `mapWithIndex` might be writeable,
  and it would just mean having to map a deck into Integers before using
  `numPow`.
* ultimately i'd like to write a different `listRangeOf` (which takes a deck of
  values and returns the deck of all permutations of lists of that deck between
  the given length, e.g., `listRangeOf (0, 10) $ from [0,1]` would be all lists
  of binary digits from length 0 to 10) that considers values 'adjacent' in
  mutation terms if you can add, remove, or change a single list item. as in,
  in the deck made by `listRangeOf (0,3) $ rangeEnum ('A', 'Z')` [] would be
  adjacent to every single-length list, and something like "AB" would be
  adjacent to "A", "B", and every string like *AB A*B AB*. at a minimum, that
  might be something i could do only given a working Alternative instance, but
  it might require more complex math, since that adjacency structure is very
  different from the axis-based ones i'm generating now

minor concerns:

* i'm fairly certain it's impossible to write a performant Monad instance,
  since it would need to evaluate every value in the deck to get the new count,
  but i'd like some confirmation of that. ditto with Foldable/Traversable
  instances, which are possible but completely impractical. i'm thinking about
  making a newtype wrapper like `SlowInstances` that would have those instances
  on it, just to prevent code from accidentally going from O(1) time complexity
  to O(n)
* `withdraw` breaks assumptions about deck structure when it removes values
  from the deck, and i don't really know how to handle that

-}

instance Functor PossibilitySpace where
  fmap f (PossibilitySpace c s g) = PossibilitySpace c s $ f . g

instance Semigroup (PossibilitySpace a) where
  (<>) = (<|>)

instance Monoid (PossibilitySpace a) where
  mempty = empty

-- one after the other
instance Applicative PossibilitySpace where
  pure v = PossibilitySpace 1 [1] $ const v
  PossibilitySpace c s f <*> PossibilitySpace c' s' g
    -- these two somewhat-contrived cases are necessary to make this officially
    -- obey the Applicative laws -- without them there's a difference between
    -- `pure f <*> pure v` and `pure (f v)`, in that the latter will have
    -- segments as [1] where the former will have it as [1,1]. ditto with
    -- `pure id <*> ...` and other formulations.
    -- that doesn't change any outputs that i'm aware of, but it does change
    -- internal state in a way that breaks the laws, and is completely
    -- unnecessary, so, here are the special cases to avoid that
    | c == 1 = f 0 <$> PossibilitySpace c' s' g
    | c' == 1 = ($ g 0) <$> PossibilitySpace c s f
    | otherwise =
      PossibilitySpace (c * c') (s <> ((c *) <$> s')) $ \i -> let
          j = i `mod` c
          k = i `div` c
        in f j $ g k


-- one or the other
{- first problem: i don't know how to merge segments here. since alternatives
   can be joining any-size enumerable decks, with any variety of segments, i'm
   not sure how exactly to store the resulting pair of segments, especially
   since those segments might contain other nested segments.
 -}
instance Alternative PossibilitySpace where
  empty = PossibilitySpace 0 [] $ const (error "no value")
  PossibilitySpace c s v <|> PossibilitySpace c' s' v' =
    PossibilitySpace (c + c') [1] $ \i -> case i - c of
      x | x >= 0 -> v' x
        | otherwise -> v i

{- second problem: this involves actually evaluating every value in the deck,
   which has a performance somewhere between "extremely bad" and
   "computer-destroying". i'm pretty sure it's not possible to make it better?
   since we need to know the total number of combinations, which we can only
   see by checking every value and adding up the individual counts, which means
   evaluating every value in the deck. making a Foldable instance would get rid
   of the `[0..c-1]` part, but not the need to fully evaluate the entire thing
-}
joinEnum :: PossibilitySpace (PossibilitySpace a) -> PossibilitySpace a
joinEnum (PossibilitySpace c s v) = mconcat $ v <$> [0..c-1]

instance Monad PossibilitySpace where
  return = pure
  e >>= f = joinEnum $ f <$> e

instance MonadPlus PossibilitySpace where
  mzero = empty
  mplus = (<|>)

-- a very slow instance. i don't know if it actually obeys any foldable laws.
instance Foldable PossibilitySpace where
  foldr f z ps = go 0 z
    where
      go i z = case select ps i of
        Nothing -> z
        Just a -> go (i+1) $ f a z

-- a very slow instance. i don't know if it actually obeys any traversable laws.
{-
instance Traversable PossibilitySpace where
  traverse f ps = go 0
    where
-}

select :: PossibilitySpace a -> Integer -> Maybe a
select enum i
  | i < 0 || i >= count enum = Nothing
  | otherwise = Just $ selector enum i

roll :: RandomGen g => PossibilitySpace a -> Rand g a
roll enum = do
  i <- liftRand $ randomR (0, count enum-1)
  return $ selector enum i

-- ideally this would return PossibilitySpace ([a], PossibilitySpace a), as in return the deck of all hands
{-
handOf :: RandomGen g => Int -> PossibilitySpace a -> Rand g ([a], PossibilitySpace a)
handOf 0 enum = return ([], enum)
handOf s enum = do
  i <- liftRand $ randomR (0, count enum-1)
  let (card, remaining) = draw enum i
  (rest, final) <- handOf (s-1) remaining
  return (card:rest, final)
-}

draw :: PossibilitySpace a -> Integer -> (a, PossibilitySpace a)
draw enum i = case select enum i of
  Nothing -> error "incoherent enumerable value"
  Just x -> (x, withdraw enum i)

oneCard :: PossibilitySpace a -> PossibilitySpace (a, PossibilitySpace a)
oneCard vs = mapWithIndex (\i v -> (v, withdraw vs i)) vs

withdraw :: PossibilitySpace a -> Integer -> PossibilitySpace a
withdraw (PossibilitySpace c s f) i = PossibilitySpace (c-1) s
  $ \i' -> if i' < i then f i' else f (i' + 1)

enumerate :: PossibilitySpace a -> [a]
enumerate enum = fromMaybe (error "incoherent enumerable value") . select enum
  <$> [0..count enum-1]

decompose :: PossibilitySpace a -> Integer -> [Integer]
decompose (PossibilitySpace _ s _) i' = snd $ mapAccumR split i' s
  where
    split :: Integer -> Integer -> (Integer, Integer)
    split i c = (i `mod` c, i `div` c)

maxAxes :: PossibilitySpace a -> [Integer]
maxAxes e = decompose e $ count e - 1

construct :: PossibilitySpace a -> [Integer] -> Integer
construct (PossibilitySpace _ s _) s' = sum $ zipWith (*) s s'

mutate :: PossibilitySpace a -> Integer -> [Integer]
mutate e i = construct e <$> mutateDecomp e (decompose e i)

mutateDecomp :: PossibilitySpace a -> [Integer] -> [[Integer]]
mutateDecomp e i = tail $ recombine $ zip i mutvals
  where
    mutvals = uncurry spread <$> zip (maxAxes e) i

mutateRow :: PossibilitySpace a -> Int -> Maybe [[Integer]]
mutateRow e i = case drop i $ maxAxes e of
  h:_ -> Just $ (flip $ setNth i) minAxes <$> [0..h]
  _ -> Nothing
  where
    minAxes :: [Integer]
    minAxes = const 0 <$> maxAxes e

setNth :: Int -> a -> [a] -> [a]
setNth i r vs = take i vs <> (r : drop (i+1) vs)

spread :: (Ord a, Num a) => a -> a -> [a]
spread m c = snd <$> filter fst
  [ (c > 0, c - 1)
  , (c < m, c + 1)
  ]

recombine :: [(a, [a])] -> [[a]]
recombine [] = [[]]
recombine ((base,vars):rs) = ((:) <$> pure base <*> recombine rs)
  <> ((:) <$> vars <*> pure (fst <$> rs))

breed :: PossibilitySpace a -> Integer -> Integer -> [Integer]
breed e i j = construct e <$> breedDecomp (decompose e i) (decompose e j)

breedDecomp :: [Integer] -> [Integer] -> [[Integer]]
breedDecomp i j = recombine $ zip i j
  where
    recombine [] = [[]]
    recombine ((a,b):rs) = (:) <$> [a,b] <*> recombine rs

-- different ways of specifying a range of values. it's assumed that these
-- atomic elements will have counts less than Int's maxBound and thus can be
-- constructed like this safely.
rangeEnum :: Enum a => (a, a) -> PossibilitySpace a
rangeEnum (lo, hi) = PossibilitySpace c [1] $ \i -> toEnum $ fromEnum lo + fromIntegral i
  where
    all = fromEnum hi - fromEnum lo + 1
    c :: Integer
    c = fromIntegral all

rangeNum :: Integral a => (a, a) -> PossibilitySpace a
rangeNum (lo, hi) = PossibilitySpace c [1] $ \i -> lo + fromIntegral i
  where
    all = hi - lo + 1
    c :: Integer
    c = fromIntegral all

bounds :: (Enum a, Bounded a) => PossibilitySpace a
bounds = rangeEnum (minBound, maxBound)

-- this does require evaluating the entire input list, so don't make it too huge
from :: [a] -> PossibilitySpace a
from vs = PossibilitySpace c [1] $ \i -> vs !! fromIntegral i
  where
    c :: Integer
    c = fromIntegral $ length vs

permutationsA :: [PossibilitySpace (a -> a)] -> PossibilitySpace a -> PossibilitySpace a
permutationsA opts v = foldr addOrNotA v opts

addOrNotA :: PossibilitySpace (a -> a) -> PossibilitySpace a -> PossibilitySpace a
addOrNotA f a = a <> (f <*> a)

permutations :: [a -> a] -> PossibilitySpace a -> PossibilitySpace a
permutations opts v = foldr addOrNot v opts

addOrNot :: (a -> a) -> PossibilitySpace a -> PossibilitySpace a
addOrNot opt v = v <> (opt <$> v)

-- TODO: should maybe should return a nested PossibilitySpace
rollsOfN :: Int -> PossibilitySpace a -> PossibilitySpace [a]
rollsOfN 0 _ = pure []
rollsOfN 1 v = fmap pure v
rollsOfN n v | n < 0 = error "rollsOfN: cannot make < 0-length list"
  | otherwise = (:) <$> v <*> rollsOfN (n - 1) v

rollsOfNRange :: (Int, Int) -> PossibilitySpace a -> PossibilitySpace [a]
rollsOfNRange (lo, hi) v
  | lo == hi = rollsOfN lo v
  | lo > hi = error "rollsOfNRange: low end of range cannot be larger than high end"
  | otherwise = rollsOfN lo v `mappend` rollsOfNRange (lo+1, hi) v

-- TODO: should maybe return a nested PossibilitySpace? that would undo the work of making this bind fast, though
drawsOfN :: Int -> PossibilitySpace a -> PossibilitySpace [a]
drawsOfN 0 _ = pure []
drawsOfN 1 v = fmap pure v
drawsOfN n v | n < 0 = error "drawsOfN: cannot make < 0-length list"
  | otherwise = (\(h, rs) -> (h :) <$> drawsOfN (n-1) rs)
      `fixedBind` oneCard v

drawsOfNRange :: (Int, Int) -> PossibilitySpace a -> PossibilitySpace [a]
drawsOfNRange (lo, hi) v
  | lo == hi = drawsOfN lo v
  | lo > hi = error "distinctRangeListOf: low end of range can't be higher than high end"
  | otherwise = drawsOfN lo v <|> drawsOfNRange (lo+1, hi) v





-- because this is only used with oneCard, we know that every nested possibility space will have the same size, so instead of needing to evaluate all of them to check we can just check the first one and do an applicative-style deck multiplication
fixedJoin :: PossibilitySpace (PossibilitySpace a) -> PossibilitySpace a
fixedJoin (PossibilitySpace c s f) = let
    c' = count $ f 0
  in PossibilitySpace (c * c') [1] $ \i -> let
      j = i `div` c'
      k = i `mod` c'
      (PossibilitySpace _ _ g) = f j
    in g k
-- this is a (hugely) faster bind for a specific kind of nesting. specifically, this is only safe to use when the resultant probability space has the same number of output values for each subdeck. if that is not true, it will corrupt your space data.
fixedBind :: (a -> PossibilitySpace b) -> PossibilitySpace a -> PossibilitySpace b
fixedBind f e = fixedJoin $ f <$> e

-- this does not check the dependency assertion, and will output various kinds of garbage if it turns out to be untrue
-- this might corrupt segment data after a use. it certainly alters the bound axis, so that binding on the same axis multiple times is quadratic for one pass / exponential for many
dependentBind :: Int -> (a -> PossibilitySpace b) -> PossibilitySpace a -> PossibilitySpace b
dependentBind ax g v@(PossibilitySpace c s f)  = PossibilitySpace c' s'
	$ \i -> let
			decomp = decompose (PossibilitySpace undefined s' undefined) i
			(j_, k_) = indexer $ decomp !! ax
			j = construct v $ setNth ax j_ decomp
			k = k_
		in g' (f j) k
	where
		g' a = selector $ g a
		c' = product $ (+ 1) <$> newAxes
		s' = rebuildSegmentsFromAxes newAxes
		indexer = bindPair sizes
		newAxes = setNth ax indexCount $ maxAxes v
		indexCount = sum sizes - 1
		axisCount = length s

		sizes :: [Integer]
		sizes = count . g <$> checkVals
		-- checkVals :: [a]
		checkVals = maybe
			(error $ unwords
				[ "bad dependent bind: asked for row"
				, show ax
				, "but given space only has"
				, show $ length $ segments v
				, "axes."
				])
			(fmap $ fromMaybe (error "bad space") . select v . construct v)
				$ mutateRow v ax

-- turns a list like [1,1,2,3] into [1,2,4,7] (turning each value into an increment of a base value rather than a size, for converting a list-of-chunk-sizes into a list-of-offsets)
asIncrementing :: Num a => [a] -> [a]
asIncrementing = reverse . snd . foldl' (\(s, cs) i -> (i + s, i + s : cs)) (0, [])

-- given a bound sizes list, plus the actual offset axis value (from decompose), return 1. the number of size values passed and 2. the index of the current size value. the latter can be directly used as an argument for g'
-- this is clearly not going to be that efficient for large axes (O(n) traversal of the axes, doing all the math manually, etc). since we have to traverse the entire axis once, to get the sizes in the first place, it might be better to accumulate them so that this can be turned into... idk, a function? `Int -> (Integer, Integer)` or something? idk. it clearly has to traverse the entire axis ONCE, because that's how it figures out the new size in the first place. but since this function would get used many times (it needs to be called with a new index for each axis value we actually evaluate) i don't know if there's a good way to reduce it.
-- also this gets called every time we want to select something from a bound value. the evaluation time adds up, and a linear traversal of the entire axis isn't particularly fast. i guess for small axes it could get automemoized, if there are only like ten integer values that get checked ever.
bindPair :: [Integer] -> Integer -> (Integer, Integer)
bindPair sizes ax = countAxes (0, ax) sizes
	where
		countAxes :: (Integer, Integer) -> [Integer] -> (Integer, Integer)
		countAxes (col, row) (adv:rs) = if row - adv < 0
			then (col, row)
			else countAxes (col + 1, row - adv) rs
		countAxes (col, row) [] = (col, row)


rebuildSegmentsFromAxes :: [Integer] -> [Integer]
rebuildSegmentsFromAxes = foldr addAxis []
	where
		addAxis :: Integer -> [Integer] -> [Integer]
		addAxis c s = [1] <> (((c+1) *) <$> s)






-- ordered draws of n
-- which i think would be like `oneCard` but instead of returning the original domain with one removed, it would be the original domain but only containing cards 'after' the drawn one (so that after each draw we can only keep drawing larger cards)

-- problem: we're back to all the decks in the bind not having the same size, so that this won't have a fast bind implementation. though they're still sizes we can calculate statically: on a deck of size n, the result of oneCardSplit will have a total size of... either tri(n-1) or tri(n), i'm not sure which, where `tri` is the triangular number function.
-- (if the deck has 10 items, then the output deck will be a deck-of-decks, still with 10 items, but the first subdeck will have 1, 9, and the second will have 1, 8, etc decreasing until the final one will have 1, 0. so the total size of all the subdecks will be 9 + 8 + 7 + ... + 2 + 1, which is the 9th triangular number. i'm not sure about `tri(n-1)` vs `tri(n)` because the TOTAL NUMBER of cards is `tri(n)` because of the removed card, i just don't know how that affects the math)
uniqueDrawsOfN :: Int -> PossibilitySpace a -> PossibilitySpace [a]
uniqueDrawsOfN 0 _ = pure []
uniqueDrawsOfN 1 v = pure <$> v
uniqueDrawsOfN i v | i < 0 = error "uniqueDrawsOfN: cannot make < 0-length list"
  | otherwise = (\(h, rs) -> (h :) <$> uniqueDrawsOfN (i-1) rs)
      =<< oneCardSplit v
  where
    oneCardSplit :: PossibilitySpace a -> PossibilitySpace (a, PossibilitySpace a)
    oneCardSplit vs = mapWithIndex (\i v -> (v, withdrawSplit vs i)) vs
    withdrawSplit :: PossibilitySpace a -> Integer -> PossibilitySpace a
    withdrawSplit (PossibilitySpace c s f) i = PossibilitySpace (c-(i+1)) s
      $ \i' -> f $ i' + (i+1)
{-
    triJoin :: PossibilitySpace (PossibilitySpace a) -> PossibilitySpace a
    -- here we're assuming that the possibility space is triangular, with the first index containing (c-1) values all the way to the last index containing 0 values. thus, the total deck size will be `tri $ c - 1`
    so to index this, `j` is the subdeck index, and `k` is the index in the selected subdeck. i don't know if there's an efficient way to do this
    triJoin (PossibilitySpace c s f) =
        PossibilitySpace (tri $ c - 1) [1] $ \i -> let
          j = i `div` c'
          k = i `mod` c'
          (PossibilitySpace _ _ g) = f j
        in g k
    -- nth triangular number
    tri n = n * (n + 1) `div` 2
    invTri n = (sqrt (8*t + 1) - 1) / 2

    triBind :: (a -> PossibilitySpace b) -> PossibilitySpace a -> PossibilitySpace b
    triBind f e = triJoin $ f <$> e

invTri :: Floating a => a -> a
invTri n = (sqrt (8*n + 1) - 1) / 2

tri :: Integral a => a -> a
tri n = n * (n + 1) `div` 2

-}

uniqueDrawsOfNRange :: (Int, Int) -> PossibilitySpace a -> PossibilitySpace [a]
uniqueDrawsOfNRange (lo, hi) v
  | lo == hi = uniqueDrawsOfN lo v
  | lo > hi = error "uniwueDrawsOfNRange: low end of range can't be higher than high end"
  | otherwise = uniqueDrawsOfN lo v <|> uniqueDrawsOfNRange (lo+1, hi) v


deckOf :: Integer -> PossibilitySpace a -> PossibilitySpace (PossibilitySpace a)
deckOf 0 _ = pure mempty
deckOf 1 v = fmap pure v
deckOf n v
  | n < 0 = error "deckOf: can't make < 0-length deck"
  | otherwise = (\c rs -> pure c <|> rs) <$> v <*> deckOf (n - 1) v

numPow :: Integer -> PossibilitySpace a -> PossibilitySpace (Integer -> a)
numPow ins outs =
  (\o i -> fromMaybe (error "numPow: outside input range") $ select o i)
    <$> outputDecks
  where
    outputDecks = deckOf ins outs


{-
assuming we have `unselect :: PossibilitySpace a -> a -> Integer` (a reverse of `select`)
we can make the function like so:

-- (it's literally impossible. consider `from [0,0,0,0]`)
unselect :: PossibilitySpace a -> a -> Integer
unselect _ _ = undefined

functionOf :: PossibilitySpace a -> PossibilitySpace b -> PossibilitySpace (a -> b)
functionOf as bs = (. unselect as) <$> numPow (count as) bs
-}

-- idk if this would be more helpful
mapWithIndex :: (Integer -> a -> b) -> PossibilitySpace a -> PossibilitySpace b
mapWithIndex f (PossibilitySpace c seg g) = PossibilitySpace c seg $ \i -> f i (g i)

{-
foo :: Integer -> Integer -> (Integer, Float)
foo t i = (t - floor x - 1, x - fromIntegral (floor x))
  where
    c = tri t
    x = invTri $ fromIntegral (c - (i + 1))

functionOf' :: PossibilitySpace a -> PossibilitySpace b -> PossibilitySpace (a -> b)
functionOf' as bs = ...
	where
		(\i f -> (select i as, f)) `mapWithIndex` foo
		foo = numPow (count as) bs :: PossibilitySpace (Integer -> b)

-}

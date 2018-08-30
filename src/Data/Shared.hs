module Data.Shared
	( Shared(..)
	, totals
	) where

import Data.Set
import qualified Data.Map as M
import Data.Map (Map(..))
import Data.Monoid

data Shared a b = Shared [a] (Set b) (Map (Set b) [a])
	deriving (Eq, Ord, Show, Read)

totals :: Shared a b -> Set b
totals (Shared _ t _) = t

-- is it functor or bifunctor? it could map things to the same value and that would require rekeying the total/partial parts

instance (Ord a, Ord b) => Monoid (Shared a b) where
	mempty = Shared [] mempty M.empty
	mappend (Shared [] n e) b | n == mempty && e == M.empty = b
	mappend a (Shared [] n e) | n == mempty && e == M.empty = a
	mappend
		(Shared firstNames t partials)
		(Shared secondNames u partials') =
			let
				stillTotal = intersection t u
				partialFirst = difference t u -- things in `t` but not in `u`
				partialSecond = difference u t -- things in `u` but not in `t`
				newTotalNames = if stillTotal == empty
					then []
					else firstNames <> secondNames
				newPartials = M.unionsWith (<>) $
					[ partials
					, partials'
					, M.singleton partialFirst firstNames
					, M.singleton partialSecond secondNames
					]
			in
				Shared newTotalNames stillTotal newPartials
			-- anything that's in the total of BOTH remains in total (unless one value is totally empty V:). things that are in the total of one or the other get moved to partials. partials remains the same, but might have values merged into it.

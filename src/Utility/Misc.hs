module Utility.Misc
	( mapAccumLM
	) where

mapAccumLM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumLM f = go
	where
		-- go :: a -> [b] -> m (a, [c])
		go zero [] = return (zero, [])
		go zero (x:xs) = do
			(one, c) <- f zero x
			(aleph, cs) <- go one xs
			return (aleph, c:cs)

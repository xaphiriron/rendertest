
module Utility.Rand
	( Rand(..)
	, module R
	, liftRand
	, runRand
	, evalRand
	, randomFromList
	, randomFromList_
	, randomFromResult_
	, randomFromWeighedList
	, retry
	) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe (listToMaybe)

import System.Random as R

newtype Rand g a = Rand { getRand :: g -> (a, g) }

instance Monad (Rand g) where
	return a = Rand $ (,) a
	(Rand r) >>= f = Rand $ uncurry ($) . first (getRand . f) . r

instance Functor (Rand g) where
	fmap f (Rand r) = Rand $ first f . r

instance Applicative (Rand g) where
	pure = return
	(<*>) mf ma = (`liftM`ma) =<< mf

instance Semigroup a => Semigroup (Rand g a) where
	(<>) = liftA2 (<>)

instance Monoid a => Monoid (Rand g a) where
	mempty = pure mempty

liftRand :: RandomGen g => (g -> (a, g)) -> Rand g a
liftRand = Rand

runRand :: g -> Rand g a -> (a, g)
runRand g (Rand r) = r g

evalRand :: g -> Rand g a -> a
evalRand g = fst . runRand g

randomFromList :: RandomGen g => [a] -> Rand g (Maybe a)
randomFromList [] = return Nothing
randomFromList xs = do
	i <- liftRand $ randomR (0, length xs - 1)
	return . Just $ xs !! i

randomFromList_ :: RandomGen g => [a] -> Rand g a
randomFromList_ [] = error "randomFromList_: empty list"
randomFromList_ xs = do
	i <- liftRand $ randomR (0, length xs - 1)
	return $ xs !! i

randomFromResult_ :: RandomGen g => Rand g [a] -> Rand g a
randomFromResult_ = (randomFromList_ =<<)

randomFromWeighedList :: (RandomGen g) => [(Rational, a)] -> Rand g (Maybe a)
randomFromWeighedList [] = return Nothing
randomFromWeighedList [(_, a)] = return $ Just a
randomFromWeighedList xs = 
	let
		w = fromRational (sum $ fst <$> xs) :: Double
		xws = scanl1 (\(q,_) (s', y) -> (s' + q, y)) xs
	in do
		p <- liftM toRational . liftRand . randomR $ (0, w)
		return . fmap snd . listToMaybe $ dropWhile (\(q, _) -> q < p) xws

retry :: Int -> (a -> Bool) -> Rand g a -> Rand g (Maybe a)
retry 0 _ _ = return Nothing
retry x f v
	| x < 0 = error "retry given negative attempts"
	| otherwise = do
		v' <- v
		if f v'
			then return $ Just v'
			else retry (x-1) f v

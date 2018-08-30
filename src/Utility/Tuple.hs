module Utility.Tuple where

liftSnd :: Functor f => (a, f b) -> f (a, b)
liftSnd (a, fb) = fmap ((,) a) fb

liftFst :: Functor f => (f a, b) -> f (a, b)
liftFst (fa, b) = fmap (flip (,) b) fa

liftTuple :: Applicative f => (f a, f b) -> f (a, b)
liftTuple (fa, fb) = (,) <$> fa <*> fb

fst_ :: (a, b, c) -> a
fst_ (a, _, _) = a

snd_ :: (a, b, c) -> b
snd_ (_, b, _) = b

thd_ :: (a, b, c) -> c
thd_ (_, _, c) = c
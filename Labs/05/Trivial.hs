module Trivial where

newtype W a = W a deriving Show

instance Functor W where
  fmap f (W a) = W (f a)

class Pointed f where
  pure :: a -> f a
  -- Free theorem:
  -- fmap g . pure === pure . g

instance Pointed W where
  pure = W

a, b :: W Int
a = pure 1
b = fmap (+1) a
s = fmap (+1)
t = s $ s a

f :: Int -> W Int
f x = W (x+1)

bind :: (a -> W b) -> W a -> W b
bind f (W a) = f a

c = bind f (f 1)

instance Monad W where
  return = W
  (W x) >>= f = f x

-- Excercises
g :: Int -> W Int -> W Int
g x = fmap (+ x)

h :: W Int -> W Int -> W Int
h wx = bind (`g` wx)

class (Functor m, Pointed m) => Monad' m where
  join :: m (m a) -> m a
  -- Theorems:
  -- join . fmap pure === id === join . pure
  -- join . fmap join === join . join
  bind' :: (a -> m b) -> m a -> m b

instance Monad' W where
  join = bind id
  bind' f = join . fmap f


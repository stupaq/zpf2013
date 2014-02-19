{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Arith where

data Zero
data Succ n

type One   = Succ Zero
type Two   = Succ One
type Three = Succ Two
type Four  = Succ Three

one   = undefined :: One
two   = undefined :: Two
three = undefined :: Three
four  = undefined :: Four

class Add a b c | a b -> c where
  add :: a -> b -> c
  add = undefined
instance Add Zero b b
instance (Add a b c) => Add (Succ a) b (Succ c)

class Mul a b c | a b -> c where
  mul :: a -> b -> c
  mul = undefined
instance Mul Zero b Zero
instance (Mul a b c, Add b c c') => Mul (Succ a) b c'

class Fac a b | a -> b where
  fac :: a -> b
  fac = undefined
instance Fac Zero One
instance (Fac a b, Mul (Succ a) b b') => Fac (Succ a) b'


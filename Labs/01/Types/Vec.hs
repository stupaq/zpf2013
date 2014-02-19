{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Vec where

import Arith

infixr 0 :>
data Vec :: * -> * -> * where
  VNil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a

vhead :: Vec (Succ n) a -> a
vhead (x :> _) = x

vtail :: Vec (Succ n) a -> Vec n a
vtail (_ :> xs) = xs

vlast :: Vec (Succ n) a -> a
vlast (x :> VNil) = x
vlast (_ :> x :> xs) = vlast (x :> xs)


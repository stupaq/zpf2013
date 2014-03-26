{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module ContErr where
import           Control.Monad.Cont
import           Control.Monad.Error.Class

{--
Error: (a + e)
CPS: ((a + e) -> r) -> r
de Morgan: (a -> r, e -> r) -> r
curry: (a -> r) -> (e -> r) -> r
--}

type CE e a r = (e -> r) -> (a -> r) -> r

cemap :: (a -> b) -> CE e a r -> CE e b r
cemap f cea ce cb = cea ce (cb . f)

cepure :: a -> CE e a r
cepure a _ ca = ca a

cebind :: CE e a r -> (a -> CE e b r) -> CE e b r
cebind cea fceb ce cb = cea ce (\a -> fceb a ce cb)

throwCE :: e -> CE e a r
throwCE e ce _ = ce e

catchCE :: CE e a r -> (e -> CE e a r) -> CE e a r
catchCE cea fcea ce ca = cea (\e -> fcea e ce ca) ca

uncurryCE :: ((e -> r) -> (a -> r) -> r) -> (e -> r, a -> r) -> r
-- uncurryCE :: CE e a r -> (e -> e, a -> r) -> r
uncurryCE cea (ce, ca) = cea ce ca

-- Prelude.either :: (e -> r) -> (a -> r) -> Either e a -> r
-- ~ ((e -> r), (a -> r)) -> Either e a -> r
coeither :: (Either e a -> r) -> (e -> r, a -> r)
coeither oea = (oea . Left, oea . Right)

morgan1 :: ((e -> r, a -> r) -> r) -> (Either e a -> r) -> r
morgan1 cea = cea . coeither

morgan2 :: ((Either e a -> r) -> r) -> (e -> r, a -> r) -> r
morgan2 oea = oea . uncurry either

-- te funkcje ustanawiaja izomorfizm
iso1 :: ((e -> r) -> (a -> r) -> r) -> (Either e a -> r) -> r
iso1 = morgan1 . uncurry

iso2 :: ((Either e a -> r) -> r) -> (e -> r) -> (a -> r) -> r
iso2 = curry . morgan2


newtype CEM e r a = CEM { runCEM :: Cont r (Either e a) }

toCEM :: CE e a r -> CEM e r a
toCEM = CEM . cont . iso1

fromCEM :: CEM e r a -> CE e a r
fromCEM = iso2 . runCont . runCEM

instance Monad (CEM e r) where
  ma >>= amb = toCEM $ fromCEM ma `cebind` (fromCEM . amb)
  return = toCEM . cepure

instance (Error e) => MonadError e (CEM e r) where
  throwError = toCEM . throwCE
  catchError ma ema = toCEM $ catchCE (fromCEM ma) (fromCEM . ema)


{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module ContState where
import           Control.Monad.Cont

class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do { s <- get; put (f s) }

type CSM s r a = Cont (s -> r) a

-- NOTE this way we can define an instance for type synonym, try swapping
-- it with (CSM s r) to feel the difference
instance MonadState s (Cont (s -> r)) where
  get = cont $ \cs s -> cs s s
  put s' = cont $ \cu _ -> cu () s'

tick :: CSM Int r ()
tick = modify (+1)

baz :: CSM Int r Int
baz = do { put 40; tick; tick; get }

test3eq42 = runCont baz const 0


module MonadicIO where
import Impure
import ContIO hiding (main)

import Control.Monad

-- Impure
type World = ()

theWorld :: World
theWorld = ()

newtype MIO a = MIO { mio :: World -> (a, World) }

instance Functor MIO where
  fmap f m = MIO $ \w -> let (a, w') = mio m w in (f a, w')

instance Monad MIO where
  (>>=) m fm = MIO $ \w -> case mio m w of
    (a, w') -> mio (fm a) w'
  return a = MIO $ \w -> (a, w)

mgetchar :: () -> MIO Char
mgetchar () = return $ igetchar ()

mputchar :: Char -> MIO ()
mputchar c = case iputchar c of
  () -> return ()

runMIO :: MIO () -> IO ()
runMIO m = case mio m theWorld of
  -- NOTE for some reason seq is not working here as expected, it looks
  -- like it does not create data dependency for the inner unit
  ((), ()) -> return ()

-- Continuation
newtype DIO a = DIO { dio :: Cont a }

instance Functor DIO where
  fmap f d = DIO $ \fail succ -> dio d fail (succ . f)

instance Monad DIO where
  (>>=) d fd = DIO $ \fail succ -> dio d fail $ \a -> dio (fd a) fail succ
  return a = DIO $ \fail succ -> succ a

dgetchar :: DIO Char
dgetchar = DIO cgetchar

dputchar :: Char -> DIO ()
dputchar = DIO . cputchar

runDIO :: DIO () -> IO ()
runDIO = runCont . dio

-- Common code
mputstring :: String -> MIO ()
mputstring = foldr ((>>) . mputchar) (return ())

-- FIXME there should be no unit since World serves as one
mcopyin :: () -> MIO ()
mcopyin () = do
  c <- mgetchar ()
  mputchar c
  unless (c == '\0') $ mcopyin ()

dputstring :: String -> DIO ()
dputstring = foldr ((>>) . dputchar) (return ())

dcopyin :: DIO ()
dcopyin = do
  c <- dgetchar
  dputchar c
  unless (c == '\0') dcopyin

main :: IO ()
--main = runMIO $ mputstring "Hello\n"
--FIXME Impure version is not working
main = runMIO $ mcopyin ()
--main = runDIO $ dputstring "Hello\n"
--main = runDIO dcopyin


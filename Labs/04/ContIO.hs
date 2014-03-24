module ContIO where
import Dialogue2

type SuccCont a = a -> Dialogue
type FailCont = IOError -> Dialogue
type Cont a = FailCont -> SuccCont a -> Dialogue

cgetchar :: Cont Char
cgetchar fail succ ~(r1 : rs) = GetChar stdin : case r1 of
  Chr c -> succ c rs
  Failure f -> fail f rs

cputchar :: Char -> Cont ()
cputchar c fail succ ~(r1 : rs) = PutChar stdout c : case r1 of
  Success -> succ () rs
  Failure f -> fail f rs

ignore :: SuccCont a
ignore = const $ const []

report :: FailCont
report f = const $ map (PutChar stdout) $ show f

runCont :: Cont () -> IO ()
runCont cont = runDialogue $ cont report ignore

cputstring :: String -> Cont ()
cputstring (c:cs) fail succ = cputchar c fail $ const $ cputstring cs fail succ
cputstring [] fail succ = succ ()

ccopyin :: Cont ()
ccopyin fail succ = cgetchar fail $ \c -> case c of
  '\0' -> succ ()
  _ -> cputchar c fail $ const $ ccopyin fail succ

main :: IO ()
--main = runCont $ cputstring "Hello\n"
main = runCont ccopyin


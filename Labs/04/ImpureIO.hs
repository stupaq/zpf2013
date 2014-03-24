module ImpureIO where
import Impure

iputstring = foldr (seq . iputchar) ()

icopyin () = case igetchar () of
  '\0' -> ()
  c -> iputchar c `seq` icopyin ()

main :: IO ()
--main = iputstring "Hello\n" `seq` return ()
main = icopyin () `seq` return ()

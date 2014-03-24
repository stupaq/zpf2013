module DialogueIO where
import Dialogue2

dprintstring cout xs = const $ map (PutChar cout) xs

dcopyin cin cout ~(Chr c : ~(Success : xs)) = GetChar cin : case c of
  '\0' -> []
  _ -> PutChar cout c : dcopyin cin cout xs

dcopyfile ~(StrList args : ~(r1 : ~(r2 : rs))) = GetArgs : case args of
  [fin, fout] -> OpenFile fin readMode : OpenFile fout writeMode : case (r1, r2) of
    (Chan cin, Chan cout) -> dcopyin cin cout rs ++ [CloseChan cin, CloseChan cout]
    f -> dprintstring stdout (show f) rs
  _ -> dprintstring stdout "Wrong number of arguments!" rs

main :: IO ()
--main = runDialogue $ dprintstring stdout "Hello\n"
--main = runDialogue $ dcopyin stdin stdout
main = runDialogue dcopyfile


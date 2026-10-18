def shiftOne : List Char -> List Char
  | [] => []
  | x :: xs => xs ++ [x]

def cycleDup (s : String) : List Char :=
  let cs := s.toList
  (cs.zip (shiftOne cs)).filter (fun (a, b) => a == b) |>.map (·.1)

def solve (input : String) : Nat :=
  let digits := cycleDup input
  digits.foldl (fun acc x => acc + (x.toNat - '0'.toNat)) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input.trimAscii.copy)

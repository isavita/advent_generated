def rotate (k : Nat) (cs : List Char) : List Char :=
  cs.drop k ++ cs.take k

def cycleDup (cs : List Char) : List Char :=
  let hlen := cs.length / 2
  (cs.zip (rotate hlen cs)).filter (fun (a, b) => a == b) |>.map (·.1)

def solve (input : String) : Nat :=
  let digits := cycleDup input.toList
  digits.foldl (fun acc x => acc + (x.toNat - '0'.toNat)) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input.trimAscii.copy)

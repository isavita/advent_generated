def solve (input : String) : Nat :=
  input
  |>.splitOn "\n"
  |>.filterMap String.toNat?
  |>.foldl (fun acc x => acc + x / 3 - 2) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

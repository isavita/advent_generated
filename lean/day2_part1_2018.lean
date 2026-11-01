def hasCount (n : Nat) (s : String) : Bool :=
  s.toList.any (fun c => s.toList.countP (· == c) == n)

def solve (input : String) : Nat :=
  let ids := (input.splitOn "\n").filter (fun l => !l.isEmpty)
  let twos   := ids.countP (hasCount 2)
  let threes := ids.countP (hasCount 3)
  twos * threes

def main : IO Unit := do
  let input ← IO.FS.readFile "input.txt"
  IO.println (solve input)

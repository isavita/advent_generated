def calcDifference (xs : List String) : Nat :=
  let ns := xs.filterMap String.toNat?
  match ns.min?, ns.max? with
    | some mn, some mx => mx - mn
    | _, _ => 0
  
def solve (input : String) : Nat :=
  input.trimAscii.copy.splitOn "\n"
    |>.foldl (fun acc row => acc + calcDifference (row.trimAscii.copy.splitOn "\t")) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

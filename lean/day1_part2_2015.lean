def findBasement (xs : List Char) (ind : Nat := 0) (acc : Int := 0) : Option Nat :=
  match xs, acc with
  | _, -1 => some ind
  | List.nil, _ => none
  | List.cons '(' xs', _ => findBasement xs' (ind + 1) (acc + 1)
  | List.cons _ xs', _ => findBasement xs' (ind + 1) (acc - 1)

def solve (input : String) : Int :=
  (findBasement input.toList).getD 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

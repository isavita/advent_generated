def diffCount : List Char → List Char → Nat
  | [], _ => 0
  | _, [] => 0
  | x :: xs, y :: ys =>
    if x != y then 1 + diffCount xs ys
    else diffCount xs ys

def commonLetters (a b : String) : String :=
  (a.toList.zip b.toList)
    |>.filter (fun (x, y) => x == y)
    |>.map (·.fst)
    |> String.ofList

def solve (input : String) : String :=
  let ids := (input.splitOn "\n").filter (fun l => !l.isEmpty)
  let pairs := ids.flatMap (fun a => ids.map (fun b => (a, b)))
  match pairs.find? (fun (a, b) => diffCount a.toList b.toList == 1) with
  | some (a, b) => commonLetters a b
  | none => "no pair found"

def main : IO Unit := do
  let input ← IO.FS.readFile "input.txt"
  IO.println (solve input)

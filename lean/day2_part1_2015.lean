def boxNeeds (l : Nat) (w : Nat) (h : Nat) : Nat :=
  let s1 := l * w
  let s2 := l * h
  let s3 := w * h
  let slack := min s1 (min s2 s3)
  2 * (s1 + s2 + s3) + slack

def parseDim (line : String) : Option (Nat × Nat × Nat) :=
  match (line.splitOn "x").map String.toNat? with
  | [some l, some w, some h] => some (l, w, h)
  | _ => none

def solve (input : String) : Nat :=
  (input.splitOn "\n").foldl (init := 0) fun acc line =>
    match parseDim line with
    | some (l, w, h) => acc + boxNeeds l w h
    | _ => acc

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

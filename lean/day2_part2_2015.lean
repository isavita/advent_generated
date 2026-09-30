def parseDim (line : String) : Option (Nat × Nat × Nat) :=
  match (line.splitOn "x").map String.toNat? with
  | [some l, some w, some h] => some (l, w, h)
  | _ => none

def ribbonWrap (l : Nat) (w : Nat) (h : Nat) : Nat :=
  let bow := l * w * h
  let a := Nat.min l w
  let tmp := Nat.max l w
  let b := Nat.min tmp h
  bow + a + a + b + b

def solve (input : String) : Nat :=
  (input.splitOn "\n")
  |>.filterMap parseDim
  |>.map (fun (l, w, h) => ribbonWrap l w h)
  |>.sum

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

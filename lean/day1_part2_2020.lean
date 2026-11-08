def findTwoSum : List Nat -> Nat -> Option (Nat × Nat)
  | [], _ => none
  | a :: tl, target =>
    match tl.find? (fun b => a + b == target) with
      | some b => some (a, b)
      | none => findTwoSum tl target

def findThreeSum : List Nat → Nat → Option (Nat × Nat × Nat)
  | [], _ => none
  | a :: rest, target =>
    match findTwoSum rest (target - a) with
    | some (b, c) => some (a, b, c)
    | none => findThreeSum rest target

def solve (input : String) : Nat :=
  let ns :=
    input
    |>.splitOn "\n"
    |>.filterMap String.toNat?

  match findThreeSum ns 2020 with
    | none => 0
    | some (a, b, c) => a * b * c

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input.trimAscii.copy)

def checkLetter (pass : String) (letter: Char) (min : Nat) (max : Nat) : Bool :=
  let count := pass.toList.count letter
  count >= min && count <= max

def isValidPassword (row : String) : Bool :=
  match row.splitOn ": " with
    | rules :: password :: [] =>
      match rules.splitOn " " with
        | range :: char :: [] =>
          match range.splitOn "-" with
          | min :: max :: [] =>
            match min.toNat?, max.toNat?, char.toList.head? with
              | some min', some max', some c =>
                checkLetter password c min' max'
              | _, _, _ => false
          | _ => false
        | _ => false
    | _ => false

def solve (input : String) : Nat :=
  input
    |>.splitOn "\n"
    |>.filter isValidPassword
    |>.length

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input.trimAscii.copy)

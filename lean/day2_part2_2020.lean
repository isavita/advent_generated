def checkLetter (pass : String) (letter: Char) (ind1 : Nat) (ind2 : Nat) : Bool :=
  let chars := pass.toList
  match chars[ind1-1]?, chars[ind2-1]? with
    | some ch1, some ch2 =>
      (ch1 == letter && ch2 != letter) || (ch1 != letter && ch2 == letter)
    | some ch, none => ch == letter
    | none, some ch => ch == letter
    | _, _ => false


def isValidPassword (row : String) : Bool :=
  match row.splitOn ": " with
    | rules :: password :: [] =>
      match rules.splitOn " " with
        | range :: char :: [] =>
          match range.splitOn "-" with
          | index1 :: index2 :: [] =>
            match index1.toNat?, index2.toNat?, char.toList.head? with
              | some ind1, some ind2, some c =>
                checkLetter password c ind1 ind2
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

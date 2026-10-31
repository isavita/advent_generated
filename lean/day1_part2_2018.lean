import Std.Data.HashMap

partial def findDup (original : List Int) (changes : List Int) (acc : Int) (seen : Std.HashSet Int) : Int :=
  match changes with
  | [] => findDup original original acc seen
  | n :: ns =>
    let acc := acc + n
    if seen.contains acc then acc
    else findDup original ns acc (seen.insert acc)

def solve (input : String) : Int :=
  let changes := (input.replace "+" "").splitOn "\n"
    |>.filterMap String.toInt?
  findDup changes changes 0 (({} : Std.HashSet Int).insert 0)

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input.trimAscii.copy)

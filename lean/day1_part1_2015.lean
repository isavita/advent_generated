def solve (input : String) : Int :=
  input.trimAscii.foldl (init := 0) fun acc x =>
    if x == '(' then acc + 1
    else acc - 1

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

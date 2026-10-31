def solve (input : String) : Int :=
  (input.replace "+" "").splitOn "\n"
  |>.filterMap String.toInt?
  |>.foldl (· + ·) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input.trimAscii.copy)

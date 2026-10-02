def isNice (line : String) : Bool :=
  let s := line.trimAscii.copy.toList
  let rec hasPair : List Char -> Bool
  | a :: b :: tl => [a, b].IsInfix tl || hasPair (b :: tl)
  | _ => false

  let rec hasGap : List Char -> Bool
  | a :: b  :: c :: tl => a == c || hasGap (b :: c :: tl)
  | _ => false

  hasPair s && hasGap s

def solve (input : String) : Nat :=
  (input.splitOn "\n").countP isNice

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

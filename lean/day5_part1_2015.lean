def isNiceStr (line : String) : Bool :=
  let s := line.trimAscii.copy
  -- s contains at least 3 of ['a', 'e', 'i', 'u', 'o'].
  -- Note: It can be the same letter repeated more than once.
  let vowels := s.toList.countP (fun
    | 'a' | 'e' | 'i' | 'u' | 'o' => true
    | _ => false)

  -- It contains at least one letter that appears twice in a row
  let rec hasAdjacentDup : List Char -> Bool
  | a :: b :: tl => a == b || hasAdjacentDup (b :: tl)
  | _ => false

  let hasDup := hasAdjacentDup s.toList

  -- It does not contain the strings "ab" "cd" "pq" "xy"
  let doesNotContain := !(["ab", "cd", "pq", "xy"].any fun sub => s.contains sub)

  vowels >= 3 && hasDup && doesNotContain

def solve (input : String) : Nat :=
  (input.splitOn "\n").foldl (init := 0) (fun acc line =>
    if isNiceStr line then acc + 1
    else acc
  )

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

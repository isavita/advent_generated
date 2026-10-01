def solve (input : String) : Nat :=
  let (_, visitedHouses) := input.trimAscii.foldl (init := (((0, 0) : (Int × Int)), ([(0, 0)] : List (Int × Int)))) (
    fun ((x, y), visited) mv =>
      let newPos := match mv with
        | '^' => (x + 1, y)
        | 'v' => (x - 1, y)
        | '>' => (x, y + 1)
        | '<' => (x, y - 1)
        | _ => (x, y)

      if List.contains visited newPos then (newPos, visited)
      else (newPos, newPos :: visited)
  )

  visitedHouses.length

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

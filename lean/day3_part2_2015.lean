import Std.Data.HashSet

def nextMove (p : (Int × Int)) (move : Char) : (Int × Int) :=
  match move with
    | '^' => (p.fst, p.snd + 1)
    | 'v' => (p.fst, p.snd - 1)
    | '>' => (p.fst + 1, p.snd)
    | '<' => (p.fst - 1, p.snd)
    | _ => p

def solve (input : String) : Nat :=
  let (_, _, _, visitedHouses) := input.trimAscii.foldl (init := (
      (true : Bool),
      ((0, 0) : Int × Int),
      ((0, 0) : Int × Int),
      (Std.HashSet.emptyWithCapacity 8192 : Std.HashSet (Int × Int))
    )) (
    fun (turn, santa, robot, visited) mv =>
      let mover := if turn then santa else robot
      let next := nextMove mover mv
      let visited' := if visited.contains next then visited else (Std.HashSet.insert visited next)
      if turn then (not turn, next, robot, visited')
      else (not turn, santa, next, visited')
    )

  visitedHouses.size

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

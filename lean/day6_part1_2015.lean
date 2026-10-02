inductive Action where
  | on
  | off
  | toggle

def parseCoord (s : String) : Option (Nat × Nat) :=
  match s.splitOn "," with
  | [x, y] => return (← x.toNat?, ← y.toNat?)
  | _ => none

def parseCoords(ss : List String) : Option ((Nat × Nat) × (Nat × Nat)) :=
  match ss with
  | frm :: "through" :: to :: [] =>
    match parseCoord frm, parseCoord to with
    | some frm', some to' => some (frm', to')
    | _, _ => none
  | _ => none

def parseMove (line : String) : Option ((Nat × Nat) × (Nat × Nat) × Action) := do
  match line.splitOn " " with
  | "turn" :: "on" :: rest =>
    let (frm, to) <- parseCoords rest
    return (frm, to, Action.on)
  | "turn" :: "off" :: rest =>
    let (frm, to) <- parseCoords rest
    return (frm, to, Action.off)
  | "toggle" :: rest =>
    let (frm, to)  <- parseCoords rest
    return (frm, to, Action.toggle)
  | _ => none

def updateRegion (grid : Array (Array Bool)) (frm to : Nat × Nat) (fn : Bool -> Bool) : Array (Array Bool) :=
  let (x1, y1) := frm
  let (x2, y2) := to
  Id.run do
    let mut grid := grid
    for x in [x1 : x2 + 1] do
      let mut row := grid[x]!
      for y in [y1 : y2 + 1] do
        row := row.set! y (fn row[y]!)
      grid := grid.set! x row
    return grid

def nextMove (grid : Array (Array Bool)) (line : String) : Array (Array Bool) :=
  match parseMove line with
  | some (frm, to, .on) => updateRegion grid frm to (fun _ => true)
  | some (frm, to, .off) => updateRegion grid frm to (fun _ => false)
  | some (frm, to, .toggle) => updateRegion grid frm to (fun x => !x)
  | _ => grid

def solve (input : String) : Nat :=
  let initialGrid : Array (Array Bool) := Array.replicate 1000 (Array.replicate 1000 false)
  let finalGrid := (input.splitOn "\n").foldl (fun g line => nextMove g line) initialGrid
  finalGrid.foldl (fun acc row => acc + row.count true) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

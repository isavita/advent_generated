inductive Action where
  | on
  | off
  | toggle
deriving Repr

def parseCoord (s : String) : Option (Nat × Nat) := 
  match s.splitOn "," with
  | [x, y] => do 
    let x' <- x.toNat?
    let y' <- y.toNat?
    return (x', y')
  | _ => none

def nextMove (s : String) : Option ((Nat × Nat) × (Nat × Nat) × Action) :=
  match s.splitOn " " with
    | ["turn", "on", frm, "through", to] => do 
      let frm' <- parseCoord frm
      let to' <- parseCoord to
      return (frm', to', Action.on)
    | ["turn", "off", frm, "through", to] => do
      let frm' <- parseCoord frm
      let to' <- parseCoord to
      return (frm', to', Action.off)
    | ["toggle", frm, "through", to] => do
      let frm' <- parseCoord frm
      let to' <- parseCoord to
      return (frm', to', Action.toggle)
    | _ => none

def updateRegion (grid : Array (Array Nat)) (frm to : Nat × Nat) (f : Nat -> Nat) : Array (Array Nat) := Id.run do
  let mut grid := grid
  let (x1, y1) := frm
  let (x2, y2) := to
  for y in [y1:y2 + 1] do
    let mut row := grid[y]!
    
    for x in [x1:x2 + 1] do
      row := row.modify x f
    grid := grid.set! y row

  return grid
  
def solve (input : String) : Nat :=
  let grid :=
    (input.splitOn "\n").foldl (init := Array.replicate 1000 (Array.replicate 1000 0)) (fun grid line =>
    match nextMove line with
      | some (frm, to, Action.on) => updateRegion grid frm to fun x => x + 1
      | some (frm, to, Action.off) => updateRegion grid frm to fun x => x - 1
      | some (frm, to, Action.toggle) => updateRegion grid frm to fun x => x + 2
      | _ => grid 
    )

  grid.foldl (fun acc row => acc + row.sum) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

def findDivResult : List Nat -> List Nat -> Nat
    | [], _ => 0 
    | hd :: tl, ns  => 
      match (tl ++ ns).find? (fun x => x % hd == 0) with
        | some x => x / hd
        | none => findDivResult tl (hd :: ns)
  
def solve (input : String) : Nat :=
  input.trimAscii.copy.splitOn "\n"
    |>.foldl (fun acc row => acc + findDivResult (row.trimAscii.copy.splitOn "\t" |>.filterMap String.toNat?) []) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

def additionalFuel : Nat -> Nat
  | 0 => 0
  | n@(_ + 1) => 
    let fuel := n / 3 - 2
    fuel + additionalFuel fuel
    
def solve (input : String) : Nat :=
  input
  |>.splitOn "\n"
  |>.filterMap String.toNat?
  |>.foldl (fun acc x => acc + additionalFuel x) 0

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

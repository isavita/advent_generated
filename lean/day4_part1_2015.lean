@[extern "lean_find_coin"]
opaque findCoinC (secret : @& String) (zeros : UInt32) : Nat

def solve (input : String) : Nat :=
  findCoinC input.trimAscii.copy 5

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

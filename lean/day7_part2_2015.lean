import Std

open Std

inductive Instr where
  | lit    (v : UInt16)
  | wire   (w : String)
  | not    (x : String)
  | and    (l r : String)
  | or     (l r : String)
  | lshift (x : String) (n : Nat)
  | rshift (x : String) (n : Nat)
  deriving Repr

abbrev Circuit := HashMap String Instr

def parseLine (line : String) : Option (String × Instr) :=
  match line.splitOn " -> " with
  | [lhs, target] =>
    match lhs.splitOn " " with
    | ["NOT", x]       => some (target, .not x)
    | [a, "AND", b]    => some (target, .and a b)
    | [a, "OR", b]     => some (target, .or a b)
    | [a, "LSHIFT", n] => (n.toNat?).map (fun n => (target, .lshift a n))
    | [a, "RSHIFT", n] => (n.toNat?).map (fun n => (target, .rshift a n))
    | [a] =>
      match a.toNat? with
      | some v => some (target, .lit (UInt16.ofNat v))
      | none   => some (target, .wire a)
    | _ => none
  | _ => none

def buildCircuit (input : String) : Circuit :=
  (input.replace "\r" "").splitOn "\n"
    |>.filter (fun l => !l.isEmpty)
    |>.filterMap parseLine
    |>.foldl (fun m (w, i) => m.insert w i) ({} : Circuit)

abbrev Cache := HashMap String UInt16

mutual

partial def valueOf (c : Circuit) (cache : IO.Ref Cache) (w : String) : IO UInt16 := do
  if let some v := (← cache.get)[w]? then
    return v
  match c[w]? with
  | none         => throw (IO.userError s!"no instruction drives wire '{w}'")
  | some instr =>
    let v ← evalInstr c cache instr
    cache.modify (·.insert w v)
    return v

partial def operand (c : Circuit) (cache : IO.Ref Cache) (x : String) : IO UInt16 :=
  match x.toNat? with
  | some n => return UInt16.ofNat n
  | none   => valueOf c cache x

partial def evalInstr (c : Circuit) (cache : IO.Ref Cache) : Instr → IO UInt16
  | .lit v      => return v
  | .wire w     => valueOf c cache w
  | .not x      => return ~~~ (← operand c cache x)
  | .and l r    => return (← operand c cache l) &&& (← operand c cache r)
  | .or l r     => return (← operand c cache l) ||| (← operand c cache r)
  | .lshift x n => return UInt16.ofNat ((← operand c cache x).toNat * 2 ^ n)
  | .rshift x n => return UInt16.ofNat ((← operand c cache x).toNat / 2 ^ n)

end

def signalOn (c : Circuit) (w : String) : IO UInt16 := do
  let cache ← IO.mkRef ({} : Cache)
  valueOf c cache w

def main : IO Unit := do
  let input ← IO.FS.readFile "input.txt"
  let circuit := buildCircuit input
  let a1 ← signalOn circuit "a"
  let circuit2 := circuit.insert "b" (.lit a1)
  let a2 ← signalOn circuit2 "a"
  IO.println s!"{a2}"

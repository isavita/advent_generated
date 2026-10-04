import Std.Data.HashMap

inductive Operand where
  | wire (name : String)
  | lit (value : UInt16)
deriving Repr, BEq

def parseOperand (s : String) : Operand :=
  match s.toNat? with
    | some n => Operand.lit n.toUInt16
    | none => Operand.wire s

inductive Gate where
  | pass (a : Operand)
  | and (l r : Operand)
  | or (l r : Operand)
  | not (a : Operand)
  | lshift (l n : Operand)
  | rshift (l n : Operand)
deriving Repr


def parseGate (tokens: List String) : Option Gate :=
  match tokens with
    | [a] => some (Gate.pass (parseOperand a))
    | ["NOT", a] => some (Gate.not (parseOperand a))
    | [a, "AND", b] => some (Gate.and (parseOperand a) (parseOperand b))
    | [a, "OR", b] => some (Gate.or (parseOperand a) (parseOperand b))
    | [a, "LSHIFT", b] => some (Gate.lshift (parseOperand a) (parseOperand b))
    | [a, "RSHIFT", b] => some (Gate.rshift (parseOperand a) (parseOperand b))
    | _ => none

abbrev State := Std.HashMap String UInt16

def evalOperand (state : State) : Operand -> Option UInt16
  | .lit n => some n
  | .wire name => state[name]?

def evalGate (state : State) : Gate -> Option UInt16
  | .pass a => do
      evalOperand state a

  | .not a => do
      let x <- evalOperand state a
      return ~~~x

  | .and a b => do
      let x <- evalOperand state a
      let y <- evalOperand state b
      return x &&& y

  | .or a b => do
      let x <- evalOperand state a
      let y <- evalOperand state b
      return x ||| y

  | .lshift a b => do
      let x <- evalOperand state a
      let n <- evalOperand state b
      return x <<< n

  | .rshift a b => do
      let x <- evalOperand state a
      let n <- evalOperand state b
      return x >>> n

def applyGate (state : State) (line : String) : State :=
  match line.splitOn " -> " with
  | [gateStr, wire] =>
      match parseGate (gateStr.splitOn " ") with
      | some gate =>
          match evalGate state gate with
          | some value => state.insert wire value
          | none => state
      | none =>
          state
  | _ =>
      state

def runPass (lines : List String) (state : State) : State :=
  lines.foldl applyGate state

def runPasses (lines : List String) : Nat → State → State
  | 0, state =>
      state
  | n + 1, state =>
      runPasses lines n (runPass lines state)

def solve (input : String) : Nat :=
  let lines := input.trimAscii.copy.splitOn "\n"
  -- At most one new dependency level needs resolving per pass.
  -- Number of wires is therefore a safe upper bound.
  let state := runPasses lines lines.length ({} : State)
  (state["a"]?.getD 0).toNat

def main : IO Unit := do
  let input <- IO.FS.readFile "input.txt"
  IO.println (solve input)

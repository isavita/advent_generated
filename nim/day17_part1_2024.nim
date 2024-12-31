
import strutils, sequtils, algorithm, os

proc getComboVal(op: int, A, B, C: int): int =
  case op
  of 0..3: op
  of 4: A
  of 5: B
  of 6: C
  else:
    raise newException(ValueError, "invalid combo operand")

proc solve() =
  let lines = readFile("input.txt").splitLines()
  var A, B, C: int
  var program: seq[int]

  for line in lines:
    let s = line.strip()
    if s.len == 0:
      continue
    if s.startsWith("Register A:"):
      A = parseInt(s.split(":")[1].strip())
    elif s.startsWith("Register B:"):
      B = parseInt(s.split(":")[1].strip())
    elif s.startsWith("Register C:"):
      C = parseInt(s.split(":")[1].strip())
    elif s.startsWith("Program:"):
      program = s.split(":")[1].strip().split(",").map(parseInt)

  var outputVals: seq[string]
  var ip = 0
  while ip < program.len:
    if ip + 1 >= program.len:
      break
    let opcode = program[ip]
    let operand = program[ip + 1]

    case opcode
    of 0:
      let den = getComboVal(operand, A, B, C)
      if den != 0:
        A = A div (1 shl den)
      else:
        A = 0
      ip += 2
    of 1:
      B = B xor operand
      ip += 2
    of 2:
      B = getComboVal(operand, A, B, C) mod 8
      ip += 2
    of 3:
      if A != 0:
        ip = operand
      else:
        ip += 2
    of 4:
      B = B xor C
      ip += 2
    of 5:
      outputVals.add($((getComboVal(operand, A, B, C) mod 8)))
      ip += 2
    of 6:
      let den = getComboVal(operand, A, B, C)
      B = A div (1 shl den)
      ip += 2
    of 7:
      let den = getComboVal(operand, A, B, C)
      C = A div (1 shl den)
      ip += 2
    else:
      break

  echo outputVals.join(",")

solve()

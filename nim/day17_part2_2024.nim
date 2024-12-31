
import strutils, sequtils, algorithm, tables

type
  Program = object
    a, b, c: int64
    program: seq[int]

proc computeOperand(val, a, b, c: int64): int64 =
  case val
  of 0, 1, 2, 3:
    result = val
  of 4:
    result = a
  of 5:
    result = b
  of 6:
    result = c
  else:
    raise newException(ValueError, "Invalid combo operand: " & $val)

proc simulateComputer(program: Program): seq[int] =
  var
    a = program.a
    b = program.b
    c = program.c
    input = program.program
    outs: seq[int] = @[]

  var i = 1
  while i <= input.len:
    let cmd = input[i - 1]
    case cmd
    of 0:
      a = a shr computeOperand(int64(input[i]), a, b, c)
    of 1:
      b = b xor int64(input[i])
    of 2:
      b = computeOperand(int64(input[i]), a, b, c) mod 8
    of 3:
      if a != 0:
        i = input[i]
    of 4:
      b = b xor c
    of 5:
      outs.add(int(computeOperand(int64(input[i]), a, b, c) mod 8))
    of 6:
      b = a shr computeOperand(int64(input[i]), a, b, c)
    of 7:
      c = a shr computeOperand(int64(input[i]), a, b, c)
    else:
      raise newException(ValueError, "Invalid opcode: " & $cmd)
    i += 2

  return outs

type Pair = tuple[a: int, b: int64]

proc check(p: Program): seq[int64] =
  var
    program = p.program
    valids: seq[int64] = @[]
    stack: seq[Pair] = @[(0, 0)]
    seen: Table[Pair, bool] = initTable[Pair, bool]()

  while stack.len > 0:
    let state = stack[^1]
    stack.del(stack.len - 1)

    if seen.contains(state):
      continue
    seen[state] = true

    let depth = state.a
    let score = state.b

    if depth == program.len:
      valids.add(score)
    else:
      for i in 0..<8:
        let newScore = i + 8 * score
        let testProgram = Program(a: newScore, b: p.b, c: p.c, program: program)
        let result = simulateComputer(testProgram)
        if result.len > 0 and result[0] == program[program.len - 1 - depth]:
          stack.add((depth + 1, newScore))
  return valids

proc main() =
  let lines = readFile("input.txt").splitLines()
  var a, b, c: int64
  var program: seq[int] = @[]

  for line in lines:
    let trimmedLine = line.strip()
    if trimmedLine.startsWith("Register A:"):
      a = trimmedLine.split(":")[1].strip().parseInt()
    elif trimmedLine.startsWith("Register B:"):
      b = trimmedLine.split(":")[1].strip().parseInt()
    elif trimmedLine.startsWith("Register C:"):
      c = trimmedLine.split(":")[1].strip().parseInt()
    elif trimmedLine.startsWith("Program:"):
      program = trimmedLine.split(":")[1].strip().split(",").map(parseInt)

  let p = Program(a: a, b: b, c: c, program: program)
  let validValues = check(p)
  let minVal = validValues.min()
  echo minVal

main()

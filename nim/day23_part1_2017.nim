import strutils, sequtils, tables

type
  Instruction = object
    op: string
    x: string
    y: string

proc parseInstruction(line: string): Instruction =
  let parts = line.split(' ')
  if parts.len == 2:
    return Instruction(op: parts[0], x: parts[1], y: "")
  else:
    return Instruction(op: parts[0], x: parts[1], y: parts[2])

proc isRegister(s: string): bool =
  return s.len == 1 and s[0] in {'a'..'h'}

proc executeInstructions(instructions: seq[Instruction]): int =
  var registers = initTable[string, int]()
  for c in 'a'..'h':
    registers[$c] = 0
  var mulCount = 0
  var pc = 0
  while pc < instructions.len:
    let instr = instructions[pc]
    case instr.op
    of "set":
      if isRegister(instr.y):
        registers[instr.x] = registers[instr.y]
      else:
        registers[instr.x] = parseInt(instr.y)
      inc pc
    of "sub":
      if isRegister(instr.y):
        registers[instr.x] -= registers[instr.y]
      else:
        registers[instr.x] -= parseInt(instr.y)
      inc pc
    of "mul":
      if isRegister(instr.y):
        registers[instr.x] *= registers[instr.y]
      else:
        registers[instr.x] *= parseInt(instr.y)
      inc mulCount
      inc pc
    of "jnz":
      var xVal: int
      if isRegister(instr.x):
        xVal = registers[instr.x]
      else:
        xVal = parseInt(instr.x)
      if xVal != 0:
        var yVal: int
        if isRegister(instr.y):
          yVal = registers[instr.y]
        else:
          yVal = parseInt(instr.y)
        pc += yVal
      else:
        inc pc
    else:
      echo "Unknown instruction: ", instr.op
  return mulCount

when isMainModule:
  let input = readFile("input.txt").splitLines()
  let instructions = input.map(parseInstruction)
  let mulCount = executeInstructions(instructions)
  echo mulCount
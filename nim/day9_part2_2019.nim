import strutils, sequtils, math

type
  Opcode = enum
    Add, Mul, Inp, Out, JmpT, JmpF, Lt, Eq, RBO, Halt
  Mode = enum
    Position, Immediate, Relative

proc getOpcode(code: int): Opcode =
  case code
  of 1: Add
  of 2: Mul
  of 3: Inp
  of 4: Out
  of 5: JmpT
  of 6: JmpF
  of 7: Lt
  of 8: Eq
  of 9: RBO
  of 99: Halt
  else: raise newException(IOError, "Unknown opcode")

proc getMode(code: int): Mode =
  case code
  of 0: Position
  of 1: Immediate
  of 2: Relative
  else: raise newException(IOError, "Unknown mode")

proc getIntcodeProgram(filename: string): seq[int] =
  let file = readFile(filename)
  let program = file.split(',')
  for code in program:
    result.add parseInt(code.strip)

proc runIntcodeProgram(program: seq[int], input: int): int =
  var memory = newSeq[int](program.len * 10)
  for i in 0..<program.len:
    memory[i] = program[i]
  var ip = 0
  var relativeBase = 0
  while true:
    let opcode = getOpcode(memory[ip] mod 100)
    let mode1 = getMode((memory[ip] div 100) mod 10)
    let mode2 = getMode((memory[ip] div 1000) mod 10)
    let mode3 = getMode((memory[ip] div 10000) mod 10)
    var param1, param2, param3: int
    case mode1
    of Position: param1 = memory[ip + 1]
    of Immediate: param1 = ip + 1
    of Relative: param1 = memory[ip + 1] + relativeBase
    case mode2
    of Position: param2 = memory[ip + 2]
    of Immediate: param2 = ip + 2
    of Relative: param2 = memory[ip + 2] + relativeBase
    case mode3
    of Position: param3 = memory[ip + 3]
    of Immediate: param3 = ip + 3
    of Relative: param3 = memory[ip + 3] + relativeBase
    case opcode
    of Add:
      memory[param3] = memory[param1] + memory[param2]
      ip += 4
    of Mul:
      memory[param3] = memory[param1] * memory[param2]
      ip += 4
    of Inp:
      memory[param1] = input
      ip += 2
    of Out:
      echo memory[param1]
      ip += 2
    of JmpT:
      if memory[param1] != 0:
        ip = memory[param2]
      else:
        ip += 3
    of JmpF:
      if memory[param1] == 0:
        ip = memory[param2]
      else:
        ip += 3
    of Lt:
      if memory[param1] < memory[param2]:
        memory[param3] = 1
      else:
        memory[param3] = 0
      ip += 4
    of Eq:
      if memory[param1] == memory[param2]:
        memory[param3] = 1
      else:
        memory[param3] = 0
      ip += 4
    of RBO:
      relativeBase += memory[param1]
      ip += 2
    of Halt:
      break
  result = memory[0]

let program = getIntcodeProgram("input.txt")
echo runIntcodeProgram(program, 2)
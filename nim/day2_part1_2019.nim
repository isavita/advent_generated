import sequtils, strutils, parsecsv

type
  Opcode = enum
    Add, Multiply, Halt

proc runProgram(program: var seq[int]): int =
  var ip = 0
  while ip < program.len:
    case program[ip]
    of 1:
      let a = program[ip + 1]
      let b = program[ip + 2]
      let c = program[ip + 3]
      program[c] = program[a] + program[b]
      ip += 4
    of 2:
      let a = program[ip + 1]
      let b = program[ip + 2]
      let c = program[ip + 3]
      program[c] = program[a] * program[b]
      ip += 4
    of 99:
      break
    else:
      raise newException(IOError, "Unknown opcode")

  return program[0]

when isMainModule:
  let input = "input.txt"
  var program = readFile(input).split(',').map(parseInt)
  program[1] = 12
  program[2] = 2
  echo runProgram(program)
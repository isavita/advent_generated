import strutils, sequtils, math

proc getMode(instruction, position: int): int =
  (instruction div (10^(position+1))) mod 10

proc getParam(program: var seq[int], pointer, mode: int): int =
  if mode == 0:
    return program[program[pointer]]
  return program[pointer]

proc runProgram(program: var seq[int], input: int): int =
  var pointer = 0
  var output = 0
  while pointer < program.len:
    let instruction = program[pointer]
    let opcode = instruction mod 100
    case opcode
    of 1, 2:
      let param1 = getParam(program, pointer+1, getMode(instruction, 1))
      let param2 = getParam(program, pointer+2, getMode(instruction, 2))
      var result: int
      if opcode == 1:
        result = param1 + param2
      else:
        result = param1 * param2
      let idx = program[pointer+3]
      program[idx] = result
      pointer += 4
    of 3:
      program[program[pointer+1]] = input
      pointer += 2
    of 4:
      output = getParam(program, pointer+1, getMode(instruction, 1))
      pointer += 2
    of 99:
      return output
    else:
      raise newException(IOError, "Unknown opcode: " & $opcode)

let file = readFile("input.txt")
var program = file.strip().split(',').map(parseInt)
echo runProgram(program, 1)
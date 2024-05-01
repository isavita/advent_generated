import sequtils, strutils, parseutils

type
  Instruction = tuple[op: string, x: string, y: string]
  Registers = array['a'..'d', int]

proc allCharsIn(s: string, chars: set[char]): bool =
  for c in s:
    if c notin chars:
      return false
  return true

proc parseInstruction(line: string): Instruction =
  let parts = line.split()
  case parts[0]
  of "cpy":
    result = (op: "cpy", x: parts[1], y: parts[2])
  of "inc":
    result = (op: "inc", x: parts[1], y: "")
  of "dec":
    result = (op: "dec", x: parts[1], y: "")
  of "jnz":
    result = (op: "jnz", x: parts[1], y: parts[2])

proc executeInstruction(registers: var Registers, instruction: Instruction, ip: var int) =
  case instruction.op
  of "cpy":
    if instruction.x.allCharsIn({'a', 'b', 'c', 'd'}):
      registers[instruction.y[0]] = registers[instruction.x[0]]
    else:
      registers[instruction.y[0]] = parseInt(instruction.x)
  of "inc":
    registers[instruction.x[0]] += 1
  of "dec":
    registers[instruction.x[0]] -= 1
  of "jnz":
    if registers[instruction.x[0]] != 0:
      ip += parseInt(instruction.y) - 1

proc executeProgram(instructions: seq[Instruction], registers: var Registers) =
  var ip = 0
  while ip < instructions.len:
    executeInstruction(registers, instructions[ip], ip)
    ip += 1

when isMainModule:
  let input = readFile("input.txt").strip().splitLines()
  let instructions = input.map(parseInstruction)
  var registers: Registers
  registers['c'] = 1 # Initialize register c to 1 for part 2
  executeProgram(instructions, registers)
  echo "Value in register a: ", registers['a']

import strutils, sequtils, os

type
  Instruction = object
    name: string
    abcValues: array[3, int]

  OpcodeComputer = object
    instructions: seq[Instruction]
    registers: array[6, int]
    instructionPointer: int

proc parseInput(input: string): OpcodeComputer =
  let lines = input.splitLines()
  var instructionPointer = parseInt(lines[0].split(' ')[1])
  var instructions: seq[Instruction] = @[]
  for l in lines[1..^1]:
    let parts = l.split(' ')
    instructions.add(Instruction(name: parts[0], abcValues: [parseInt(parts[1]), parseInt(parts[2]), parseInt(parts[3])]))
  result = OpcodeComputer(instructions: instructions, instructionPointer: instructionPointer)

proc tick(o: var OpcodeComputer): bool =
  if o.registers[o.instructionPointer] >= o.instructions.len:
    echo "Out of range instruction, terminating..."
    return true
  let instIndex = o.registers[o.instructionPointer]
  let inst = o.instructions[instIndex]
  case inst.name
  of "addr": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]] + o.registers[inst.abcValues[1]]
  of "addi": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]] + inst.abcValues[1]
  of "mulr": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]] * o.registers[inst.abcValues[1]]
  of "muli": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]] * inst.abcValues[1]
  of "banr": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]] and o.registers[inst.abcValues[1]]
  of "bani": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]] and inst.abcValues[1]
  of "borr": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]] or o.registers[inst.abcValues[1]]
  of "bori": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]] or inst.abcValues[1]
  of "setr": o.registers[inst.abcValues[2]] = o.registers[inst.abcValues[0]]
  of "seti": o.registers[inst.abcValues[2]] = inst.abcValues[0]
  of "gtir": o.registers[inst.abcValues[2]] = int(inst.abcValues[0] > o.registers[inst.abcValues[1]])
  of "gtri": o.registers[inst.abcValues[2]] = int(o.registers[inst.abcValues[0]] > inst.abcValues[1])
  of "gtrr": o.registers[inst.abcValues[2]] = int(o.registers[inst.abcValues[0]] > o.registers[inst.abcValues[1]])
  of "eqir": o.registers[inst.abcValues[2]] = int(inst.abcValues[0] == o.registers[inst.abcValues[1]])
  of "eqri": o.registers[inst.abcValues[2]] = int(o.registers[inst.abcValues[0]] == inst.abcValues[1])
  of "eqrr": o.registers[inst.abcValues[2]] = int(o.registers[inst.abcValues[0]] == o.registers[inst.abcValues[1]])
  o.registers[o.instructionPointer] += 1
  return o.registers[o.instructionPointer] >= o.instructions.len

proc solve(input: string): int =
  var opcodeComputer = parseInput(input)
  while not opcodeComputer.tick():
    if opcodeComputer.registers[opcodeComputer.instructionPointer] == 28:
      break
  return opcodeComputer.registers[5]

let input = readFile("input.txt")
echo solve(input)

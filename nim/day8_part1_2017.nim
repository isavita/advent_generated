import strutils, sequtils, tables

type
  Instruction = object
    register: string
    operation: string
    amount: int
    condition: string

proc parseInstruction(line: string): Instruction =
  let parts = line.split(" ")
  let register = parts[0]
  let operation = parts[1]
  let amount = parseInt(parts[2])
  let condition = join(parts[4..^1], " ")
  Instruction(register: register, operation: operation, amount: amount, condition: condition)

proc evaluateCondition(registers: Table[string, int], condition: string): bool =
  let parts = condition.split(" ")
  let reg = parts[0]
  let op = parts[1]
  let value = parseInt(parts[2])
  let regValue = registers.getOrDefault(reg, 0)
  case op
  of ">": return regValue > value
  of "<": return regValue < value
  of ">=": return regValue >= value
  of "<=": return regValue <= value
  of "==": return regValue == value
  of "!=": return regValue != value
  else: return false

proc executeInstructions(instructions: seq[Instruction]): int =
  var registers = initTable[string, int]()
  for instr in instructions:
    if evaluateCondition(registers, instr.condition):
      if instr.operation == "inc":
        registers[instr.register] = registers.getOrDefault(instr.register, 0) + instr.amount
      else:
        registers[instr.register] = registers.getOrDefault(instr.register, 0) - instr.amount
  registers.values().toSeq.max

when isMainModule:
  let input = readFile("input.txt").splitLines().map(parseInstruction)
  echo executeInstructions(input)
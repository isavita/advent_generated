
import strutils, tables

proc main() =
  let instructions = readFile("input.txt").strip().splitLines()
  var registers = initTable[string, int]()
  var currentInstruction = 0
  var lastSound = 0

  # Helper proc to get value (either literal int or register content)
  proc getValue(operand: string, regs: Table[string, int]): int =
    if operand[0].isAlphaAscii():
      result = regs.getOrDefault(operand, 0) # Default to 0 if register not set
    else:
      result = parseInt(operand)

  while currentInstruction >= 0 and currentInstruction < instructions.len:
    let parts = instructions[currentInstruction].splitWhitespace()
    let command = parts[0]
    let register = parts[1]

    var value = 0 # Initialize value, used by commands with 3 parts
    if parts.len == 3:
      value = getValue(parts[2], registers)

    # Get the value of the primary register (first operand) if it's needed
    # Note: 'set' doesn't need the old value, but others do.
    # Fetching it here is convenient. jgz uses its *own* logic for the condition value.
    let registerValue = registers.getOrDefault(register, 0)

    var jumped = false
    case command
    of "snd":
      lastSound = registerValue
    of "set":
      registers[register] = value
    of "add":
      registers[register] = registerValue + value
    of "mul":
      registers[register] = registerValue * value
    of "mod":
      registers[register] = registerValue mod value
    of "rcv":
      if registerValue != 0:
        echo lastSound
        return # Exit main proc, effectively breaking the loop
    of "jgz":
      # jgz checks the value of its *first* operand
      let conditionValue = getValue(register, registers)
      if conditionValue > 0:
        currentInstruction += value
        jumped = true # Mark that we jumped, skip increment
    else:
      # Optional: Handle unknown command if necessary
      stderr.writeLine("Unknown command: " & command)
      return

    if not jumped:
      currentInstruction += 1

when isMainModule:
  main()

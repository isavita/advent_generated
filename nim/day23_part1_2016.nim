
import strutils, tables, os

proc readInstructions(filename: string): seq[string] =
  result = readFile(filename).splitLines()

proc getValue(s: string, registers: var Table[string, int]): int =
  try:
    return parseInt(s)
  except ValueError:
    return registers[s]

proc toggleInstruction(instr: string): string =
  let parts = instr.splitWhitespace()
  case parts[0]
  of "inc": return "dec " & parts[1]
  of "dec", "tgl": return "inc " & parts[1]
  of "jnz": return "cpy " & parts[1] & " " & parts[2]
  of "cpy": return "jnz " & parts[1] & " " & parts[2]
  else: return instr

proc executeInstructions(instructions: var seq[string], registers: var Table[string, int]) =
  var pc = 0
  while pc < instructions.len:
    let fields = instructions[pc].splitWhitespace()
    case fields[0]
    of "cpy":
      let x = getValue(fields[1], registers)
      if fields[2] in registers:
        registers[fields[2]] = x
    of "inc":
      if fields[1] in registers:
        inc registers[fields[1]]
    of "dec":
      if fields[1] in registers:
        dec registers[fields[1]]
    of "jnz":
      let x = getValue(fields[1], registers)
      if x != 0:
        pc += getValue(fields[2], registers) - 1
    of "tgl":
      let x = getValue(fields[1], registers)
      let tgt = pc + x
      if tgt >= 0 and tgt < instructions.len:
        instructions[tgt] = toggleInstruction(instructions[tgt])
    inc pc

proc main() =
  var instructions = readInstructions("input.txt")
  var registers = {"a": 7, "b": 0, "c": 0, "d": 0}.toTable()
  executeInstructions(instructions, registers)
  echo registers["a"]

main()

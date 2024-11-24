
import std/[strutils, parseutils, tables, os]

proc getValue(s: string, registers: var Table[string, int]): int =
  try:
    return parseInt(s)
  except ValueError:
    return registers[s]

proc producesClockSignal(a: int, instructions: seq[string]): bool =
  var registers = {"a": a, "b": 0, "c": 0, "d": 0}.toTable
  var lastOutput, outputCount: int

  var i = 0
  while i < instructions.len:
    let parts = instructions[i].split()
    case parts[0]
    of "cpy":
      registers[parts[2]] = getValue(parts[1], registers)
    of "inc":
      inc registers[parts[1]]
    of "dec":
      dec registers[parts[1]]
    of "jnz":
      let val = getValue(parts[1], registers)
      if val != 0:
        let jump = parseInt(parts[2])
        i += jump
        continue
    of "out":
      let val = getValue(parts[1], registers)
      if val notin {0, 1}:
        return false
      if outputCount > 0 and val == lastOutput:
        return false
      lastOutput = val
      inc outputCount
      if outputCount > 50:
        return true
    inc i

  return false

proc main() =
  let file = open("input.txt")
  defer: file.close()

  var instructions: seq[string]
  for line in file.lines:
    instructions.add(line)

  for a in 1 .. int.high:
    if producesClockSignal(a, instructions):
      echo a
      break

main()

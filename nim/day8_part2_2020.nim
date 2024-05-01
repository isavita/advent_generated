import strutils, sequtils, strformat

proc parseInstruction(instruction: string): tuple[op: string, arg: int] =
  let parts = instruction.split()
  let op = parts[0]
  let arg = parseInt(parts[1])
  (op, arg)

proc executeBootCode(instructions: seq[string]): tuple[accumulator: int, terminated: bool] =
  var accumulator = 0
  var visited = newSeq[bool](instructions.len)
  var currentInstruction = 0

  while currentInstruction < instructions.len:
    if visited[currentInstruction]:
      return (accumulator, false)
    visited[currentInstruction] = true
    let (op, arg) = parseInstruction(instructions[currentInstruction])
    case op
    of "acc":
      accumulator += arg
      inc currentInstruction
    of "jmp":
      currentInstruction += arg
    of "nop":
      inc currentInstruction
  (accumulator, true)

when isMainModule:
  let file = readFile("input.txt")
  let instructions = file.splitLines()
  for i in 0..<instructions.len:
    let (op, arg) = parseInstruction(instructions[i])
    if op == "acc":
      continue
    var modifiedInstructions = instructions
    if op == "jmp":
      modifiedInstructions[i] = fmt"nop {arg}"
    else:
      modifiedInstructions[i] = fmt"jmp {arg}"
    let (accumulator, terminated) = executeBootCode(modifiedInstructions)
    if terminated:
      echo accumulator
      break
import strutils, sequtils, os

type Instruction = tuple[op: string, arg: int]
type Program = seq[Instruction]

proc parseInstruction(line: string): Instruction =
  let parts = line.splitWhitespace()
  let op = parts[0]
  let arg = parseInt(parts[1])
  (op, arg)

proc loadProgram(filename: string): Program =
  let lines = readFile(filename).splitLines()
  for line in lines:
    result.add(parseInstruction(line))

proc execute(program: Program): int =
  var acc = 0
  var ip = 0
  var visited = newSeq[bool](program.len)
  while ip < program.len:
    if visited[ip]:
      return acc
    visited[ip] = true
    let (op, arg) = program[ip]
    case op
    of "acc":
      acc += arg
      ip += 1
    of "jmp":
      ip += arg
    of "nop":
      ip += 1
    else:
      raise newException(IOError, "Unknown operation")
  acc

let program = loadProgram("input.txt")
echo execute(program)
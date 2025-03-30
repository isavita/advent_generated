
import sequtils, strutils, os

type Registers = array[4, int]
type Instruction = array[4, int]
type Operation = proc(regs: var Registers, A, B, C: int)

proc addr(regs: var Registers, A, B, C: int) = regs[C] = regs[A] + regs[B]
proc addi(regs: var Registers, A, B, C: int) = regs[C] = regs[A] + B
proc mulr(regs: var Registers, A, B, C: int) = regs[C] = regs[A] * regs[B]
proc muli(regs: var Registers, A, B, C: int) = regs[C] = regs[A] * B
proc banr(regs: var Registers, A, B, C: int) = regs[C] = regs[A] and regs[B]
proc bani(regs: var Registers, A, B, C: int) = regs[C] = regs[A] and B
proc borr(regs: var Registers, A, B, C: int) = regs[C] = regs[A] or regs[B]
proc bori(regs: var Registers, A, B, C: int) = regs[C] = regs[A] or B
proc setr(regs: var Registers, A, B, C: int) = regs[C] = regs[A]
proc seti(regs: var Registers, A, B, C: int) = regs[C] = A
proc gtir(regs: var Registers, A, B, C: int) = regs[C] = if A > regs[B]: 1 else: 0
proc gtri(regs: var Registers, A, B, C: int) = regs[C] = if regs[A] > B: 1 else: 0
proc gtrr(regs: var Registers, A, B, C: int) = regs[C] = if regs[A] > regs[B]: 1 else: 0
proc eqir(regs: var Registers, A, B, C: int) = regs[C] = if A == regs[B]: 1 else: 0
proc eqri(regs: var Registers, A, B, C: int) = regs[C] = if regs[A] == B: 1 else: 0
proc eqrr(regs: var Registers, A, B, C: int) = regs[C] = if regs[A] == regs[B]: 1 else: 0

const operations: array[16, Operation] = [
  addr, addi, mulr, muli, banr, bani, borr, bori,
  setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
]

proc parseRegisters(s: string): Registers =
  let parts = s[s.find('[')+1 ..< s.find(']')].split(", ")
  for i in 0..3:
    result[i] = parseInt(parts[i])

proc parseInstruction(s: string): Instruction =
  let parts = s.split(" ")
  for i in 0..3:
    result[i] = parseInt(parts[i])

proc main() =
  type Sample = tuple[before: Registers, instruction: Instruction, after: Registers]
  var samples: seq[Sample] = @[]

  let lines = readFile("input.txt").splitLines()
  var i = 0
  while i < lines.len:
    if lines[i].startsWith("Before:"):
      let before = parseRegisters(lines[i])
      let instruction = parseInstruction(lines[i+1])
      let after = parseRegisters(lines[i+2])
      samples.add((before, instruction, after))
      i += 3 # Skip Before, Instruction, After lines
    else:
      i += 1 # Skip blank lines or other lines

  var count = 0
  for sample in samples:
    let (before, instruction, after) = sample
    let (_, A, B, C) = (instruction[0], instruction[1], instruction[2], instruction[3])
    var matches = 0
    for op in operations:
      var currentRegs = before # Array assignment creates a copy
      op(currentRegs, A, B, C)
      if currentRegs == after:
        matches += 1
    if matches >= 3:
      count += 1

  echo count

when isMainModule:
  main()

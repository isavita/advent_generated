
import strutils, sequtils, parseutils, tables, sets, os

type
  Registers = array[4, int]
  Instruction = array[4, int]
  Sample = tuple[before: Registers, instruction: Instruction, after: Registers]
  OpcodeProc = proc (r: var Registers, a, b, c: int)

proc addr(r: var Registers, a, b, c: int) = r[c] = r[a] + r[b]
proc addi(r: var Registers, a, b, c: int) = r[c] = r[a] + b
proc mulr(r: var Registers, a, b, c: int) = r[c] = r[a] * r[b]
proc muli(r: var Registers, a, b, c: int) = r[c] = r[a] * b
proc banr(r: var Registers, a, b, c: int) = r[c] = r[a] and r[b]
proc bani(r: var Registers, a, b, c: int) = r[c] = r[a] and b
proc borr(r: var Registers, a, b, c: int) = r[c] = r[a] or r[b]
proc bori(r: var Registers, a, b, c: int) = r[c] = r[a] or b
proc setr(r: var Registers, a, b, c: int) = r[c] = r[a]
proc seti(r: var Registers, a, b, c: int) = r[c] = a
proc gtir(r: var Registers, a, b, c: int) = r[c] = if a > r[b]: 1 else: 0
proc gtri(r: var Registers, a, b, c: int) = r[c] = if r[a] > b: 1 else: 0
proc gtrr(r: var Registers, a, b, c: int) = r[c] = if r[a] > r[b]: 1 else: 0
proc eqir(r: var Registers, a, b, c: int) = r[c] = if a == r[b]: 1 else: 0
proc eqri(r: var Registers, a, b, c: int) = r[c] = if r[a] == b: 1 else: 0
proc eqrr(r: var Registers, a, b, c: int) = r[c] = if r[a] == r[b]: 1 else: 0

const opcodes: array[16, OpcodeProc] = [
  addr, addi, mulr, muli, banr, bani, borr, bori,
  setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
]

proc testOpcode(sample: Sample, op: OpcodeProc): bool =
  var currentRegisters = sample.before
  op(currentRegisters, sample.instruction[1], sample.instruction[2], sample.instruction[3])
  return currentRegisters == sample.after

proc parseRegisters(line: string): Registers =
  let parts = line[9 .. ^2].split(", ")
  for i in 0..3:
    result[i] = parseInt(parts[i])

proc parseInstruction(line: string): Instruction =
   let parts = line.split(" ")
   for i in 0..3:
     result[i] = parseInt(parts[i])

proc readInput(filename: string): (seq[Sample], seq[Instruction]) =
  var samples: seq[Sample]
  var program: seq[Instruction]
  var lines = readFile(filename).strip.splitLines
  var i = 0
  while i < lines.len:
    if lines[i].startsWith("Before"):
      let before = parseRegisters(lines[i])
      i += 1
      let instruction = parseInstruction(lines[i])
      i += 1
      let after = parseRegisters(lines[i])
      i += 2 # Skip After line and blank line
      samples.add((before, instruction, after))
    elif lines[i].len > 0 and lines[i][0].isDigit():
       # Assume the rest is the program after the samples and blank lines
       while i < lines.len and lines[i].len > 0:
           program.add(parseInstruction(lines[i]))
           i += 1
       break # Program found and parsed, exit outer loop
    else:
      i += 1 # Skip blank lines between samples or before program
  return (samples, program)

proc partOne(samples: seq[Sample]): int =
  for sample in samples:
    var numBehaveLike = 0
    for op in opcodes:
      if testOpcode(sample, op):
        numBehaveLike += 1
    if numBehaveLike >= 3:
      result += 1

proc partTwo(samples: seq[Sample], program: seq[Instruction]): int =
  var possibleOpcodes = initTable[int, HashSet[OpcodeProc]]()
  var opcodeMapping = initTable[int, OpcodeProc]()

  for sample in samples:
    let opcodeNum = sample.instruction[0]
    var matchingOps = initHashSet[OpcodeProc]()
    for op in opcodes:
      if testOpcode(sample, op):
        matchingOps.incl(op)

    if possibleOpcodes.hasKey(opcodeNum):
        possibleOpcodes[opcodeNum] = possibleOpcodes[opcodeNum] * matchingOps # Intersection
    else:
        possibleOpcodes[opcodeNum] = matchingOps

  # Resolve mappings
  var foundMapping = true
  while opcodeMapping.len < 16 and foundMapping:
      foundMapping = false
      var foundOp: OpcodeProc = nil
      for num, ops in possibleOpcodes.pairs:
          if num notin opcodeMapping and ops.len == 1:
              for op in ops: foundOp = op # Get the single element
              opcodeMapping[num] = foundOp
              foundMapping = true
              # Remove this found op from all other possibilities
              for otherNum in possibleOpcodes.keys:
                  if otherNum != num:
                      possibleOpcodes[otherNum].excl(foundOp)
              break # Restart search for singles after modification


  var registers: Registers # Initialized to zeros
  for instruction in program:
    let op = opcodeMapping[instruction[0]]
    op(registers, instruction[1], instruction[2], instruction[3])

  return registers[0]

when isMainModule:
  let (samples, program) = readInput("input.txt")
  echo partOne(samples)
  echo partTwo(samples, program)


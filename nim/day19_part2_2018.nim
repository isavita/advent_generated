
import strutils, sequtils, re, math, os

proc executeOp(op: string, a, b, c: int, r: var array[6, int]) =
  case op:
  of "addr": r[c] = r[a] + r[b]
  of "addi": r[c] = r[a] + b
  of "mulr": r[c] = r[a] * r[b]
  of "muli": r[c] = r[a] * b
  of "banr": r[c] = r[a] and r[b]
  of "bani": r[c] = r[a] and b
  of "borr": r[c] = r[a] or r[b]
  of "bori": r[c] = r[a] or b
  of "setr": r[c] = r[a]
  of "seti": r[c] = a
  of "gtir": r[c] = if a > r[b]: 1 else: 0
  of "gtri": r[c] = if r[a] > b: 1 else: 0
  of "gtrr": r[c] = if r[a] > r[b]: 1 else: 0
  of "eqir": r[c] = if a == r[b]: 1 else: 0
  of "eqri": r[c] = if r[a] == b: 1 else: 0
  of "eqrr": r[c] = if r[a] == r[b]: 1 else: 0
  else: discard

proc sumOfDivisors(n: int): int =
  result = 0
  for i in 1..n:
    if n mod i == 0:
      result += i

when isMainModule:
  let content = readFile("input.txt")
  let lines = content.strip().splitLines()

  var ipRegister = 0
  var program: seq[tuple[op: string, a, b, c: int]] = @[]
  var programLines = lines

  if lines[0].startsWith("#ip"):
    ipRegister = lines[0].findAll(re"\d+")[0].parseInt()
    programLines = lines[1..^1]

  for line in programLines:
    let parts = line.splitWhitespace()
    let op = parts[0]
    let nums = line.findAll(re"\d+").map(parseInt)
    program.add((op: op, a: nums[0], b: nums[1], c: nums[2]))

  var registers = [1, 0, 0, 0, 0, 0] # Initialize R0 = 1 directly
  var ip = 0
  var cycles = 0
  let maxCycles = 1000 # Match Python's arbitrary limit

  while ip >= 0 and ip < program.len and cycles < maxCycles:
      registers[ipRegister] = ip
      let inst = program[ip]
      executeOp(inst.op, inst.a, inst.b, inst.c, registers)
      ip = registers[ipRegister] + 1
      cycles += 1

  # Optimization: The program calculates the sum of divisors of a number N.
  # Running for 1000 cycles with R0=1 is enough to find N in the registers.
  let n = registers.max()
  echo sumOfDivisors(n)


import strutils, sequtils, os

type Instruction = tuple[opcode: string, a, b, c: int]

proc execute(opcode: string, a, b, c: int, registers: var array[6, int]) =
  case opcode
  of "addr": registers[c] = registers[a] + registers[b]
  of "addi": registers[c] = registers[a] + b
  of "mulr": registers[c] = registers[a] * registers[b]
  of "muli": registers[c] = registers[a] * b
  of "banr": registers[c] = registers[a] and registers[b]
  of "bani": registers[c] = registers[a] and b
  of "borr": registers[c] = registers[a] or registers[b]
  of "bori": registers[c] = registers[a] or b
  of "setr": registers[c] = registers[a]
  of "seti": registers[c] = a
  of "gtir": registers[c] = if a > registers[b]: 1 else: 0
  of "gtri": registers[c] = if registers[a] > b: 1 else: 0
  of "gtrr": registers[c] = if registers[a] > registers[b]: 1 else: 0
  of "eqir": registers[c] = if a == registers[b]: 1 else: 0
  of "eqri": registers[c] = if registers[a] == b: 1 else: 0
  of "eqrr": registers[c] = if registers[a] == registers[b]: 1 else: 0
  else: discard

proc parseInput(filename: string): (int, seq[Instruction]) =
  let file = readFile(filename)
  let lines = file.splitLines()
  let ip = parseInt(lines[0].split()[1])
  var instructions: seq[Instruction] = @[]
  for line in lines[1..^1]:
    let parts = line.split()
    let opcode = parts[0]
    let a = parseInt(parts[1])
    let b = parseInt(parts[2])
    let c = parseInt(parts[3])
    instructions.add((opcode, a, b, c))
  (ip, instructions)

proc main() =
  let (ipRegister, instructions) = parseInput("input.txt")
  var registers: array[6, int] = [0, 0, 0, 0, 0, 0]
  var ip = 0

  while ip >= 0 and ip < instructions.len:
    registers[ipRegister] = ip
    let (opcode, a, b, c) = instructions[ip]
    execute(opcode, a, b, c, registers)
    ip = registers[ipRegister]
    ip += 1

  echo "Value in register 0: ", registers[0]

when isMainModule:
  main()
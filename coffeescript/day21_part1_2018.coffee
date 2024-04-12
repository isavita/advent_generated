fs = require 'fs'

opcodeFuncs =
  addr: (regs, a, b, c) -> regs[c] = regs[a] + regs[b]
  addi: (regs, a, b, c) -> regs[c] = regs[a] + b
  mulr: (regs, a, b, c) -> regs[c] = regs[a] * regs[b]
  muli: (regs, a, b, c) -> regs[c] = regs[a] * b
  banr: (regs, a, b, c) -> regs[c] = regs[a] & regs[b]
  bani: (regs, a, b, c) -> regs[c] = regs[a] & b
  borr: (regs, a, b, c) -> regs[c] = regs[a] | regs[b]
  bori: (regs, a, b, c) -> regs[c] = regs[a] | b
  setr: (regs, a, b, c) -> regs[c] = regs[a]
  seti: (regs, a, b, c) -> regs[c] = a
  gtir: (regs, a, b, c) -> regs[c] = if a > regs[b] then 1 else 0
  gtri: (regs, a, b, c) -> regs[c] = if regs[a] > b then 1 else 0
  gtrr: (regs, a, b, c) -> regs[c] = if regs[a] > regs[b] then 1 else 0
  eqir: (regs, a, b, c) -> regs[c] = if a == regs[b] then 1 else 0
  eqri: (regs, a, b, c) -> regs[c] = if regs[a] == b then 1 else 0
  eqrr: (regs, a, b, c) -> regs[c] = if regs[a] == regs[b] then 1 else 0

parseInput = (input) ->
  lines = input.trim().split '\n'
  instructionPointer = parseInt lines[0].split(' ')[1]
  instructions = (line.split(' ') for line in lines[1..])
  {instructions, instructionPointer}

tick = (cpu) ->
  {instructions, instructionPointer, registers} = cpu
  if registers[instructionPointer] >= instructions.length
    console.log "Out of range instruction, terminating..."
    return true

  inst = instructions[registers[instructionPointer]]
  [name, a, b, c] = inst
  a = parseInt a
  b = parseInt b
  c = parseInt c
  opcodeFuncs[name](registers, a, b, c)
  registers[instructionPointer]++
  return registers[instructionPointer] >= instructions.length

solve = (input) ->
  cpu = parseInput input
  cpu.registers = [0, 0, 0, 0, 0, 0]
  while not tick(cpu)
    if cpu.registers[cpu.instructionPointer] == 28
      break
  cpu.registers[5]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    throw err
  console.log solve(data)
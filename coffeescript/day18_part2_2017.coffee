
fs = require 'fs'

# Helper function to get the value of a register or a number
getValue = (registers, val) ->
  if isNaN(parseInt(val))
    registers[val] or 0
  else
    parseInt(val)

# Part 1
solvePart1 = (instructions) ->
  registers = {}
  lastSound = 0
  pc = 0

  while pc >= 0 and pc < instructions.length
    [instr, x, y] = instructions[pc].split ' '

    switch instr
      when 'snd'
        lastSound = getValue(registers, x)
      when 'set'
        registers[x] = getValue(registers, y)
      when 'add'
        registers[x] = (registers[x] or 0) + getValue(registers, y)
      when 'mul'
        registers[x] = (registers[x] or 0) * getValue(registers, y)
      when 'mod'
        registers[x] = (registers[x] or 0) % getValue(registers, y)
      when 'rcv'
        if getValue(registers, x) != 0
          return lastSound
      when 'jgz'
        if getValue(registers, x) > 0
          pc += getValue(registers, y) - 1

    pc++
  return null

# Part 2
solvePart2 = (instructions) ->
  registers = [{}, {}]
  queues = [[], []]
  pcs = [0, 0]
  currentProgram = 0
  sendCount = [0, 0]
  terminated = [false, false]

  registers[0]['p'] = 0
  registers[1]['p'] = 1

  while not (terminated[0] and terminated[1])
    if terminated[currentProgram]
        currentProgram = 1 - currentProgram
        continue

    pc = pcs[currentProgram]

    if pc < 0 or pc >= instructions.length
      terminated[currentProgram] = true
      currentProgram = 1 - currentProgram
      continue

    [instr, x, y] = instructions[pc].split ' '
    otherProgram = 1 - currentProgram

    switch instr
      when 'snd'
        queues[otherProgram].push getValue(registers[currentProgram], x)
        sendCount[currentProgram]++
      when 'set'
        registers[currentProgram][x] = getValue(registers[currentProgram], y)
      when 'add'
        registers[currentProgram][x] = (registers[currentProgram][x] or 0) + getValue(registers[currentProgram], y)
      when 'mul'
        registers[currentProgram][x] = (registers[currentProgram][x] or 0) * getValue(registers[currentProgram], y)
      when 'mod'
        registers[currentProgram][x] = (registers[currentProgram][x] or 0) % getValue(registers[currentProgram], y)
      when 'rcv'
        if queues[currentProgram].length > 0
          registers[currentProgram][x] = queues[currentProgram].shift()
        else
          # Check for deadlock
          if (queues[otherProgram].length is 0 and instructions[pcs[otherProgram]].startsWith('rcv'))
              terminated[0] = true
              terminated[1] = true
          else
              currentProgram = otherProgram
              continue
          
      when 'jgz'
        if getValue(registers[currentProgram], x) > 0
          pcs[currentProgram] += getValue(registers[currentProgram], y) - 1

    pcs[currentProgram]++

  return sendCount[1]

# Read input from file
input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

# Solve and print results
console.log "Part 1: #{solvePart1(input)}"
console.log "Part 2: #{solvePart2(input)}"

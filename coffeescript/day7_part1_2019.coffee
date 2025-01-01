
fs = require 'fs'

intcode = (program, inputs) ->
  memory = program.slice()
  pc = 0
  output = []
  inputIndex = 0

  getValue = (mode, value) ->
    switch mode
      when 0 then memory[value]
      when 1 then value
      else throw new Error "Invalid mode: #{mode}"

  while true
    instruction = memory[pc]
    opcode = instruction % 100
    mode1 = Math.floor(instruction / 100) % 10
    mode2 = Math.floor(instruction / 1000) % 10
    mode3 = Math.floor(instruction / 10000) % 10

    switch opcode
      when 1
        val1 = getValue(mode1, memory[pc + 1])
        val2 = getValue(mode2, memory[pc + 2])
        memory[memory[pc + 3]] = val1 + val2
        pc += 4
      when 2
        val1 = getValue(mode1, memory[pc + 1])
        val2 = getValue(mode2, memory[pc + 2])
        memory[memory[pc + 3]] = val1 * val2
        pc += 4
      when 3
        if inputIndex >= inputs.length
          return { output, halted: false, pc, memory }
        memory[memory[pc + 1]] = inputs[inputIndex]
        inputIndex += 1
        pc += 2
      when 4
        output.push getValue(mode1, memory[pc + 1])
        pc += 2
      when 5
        val1 = getValue(mode1, memory[pc + 1])
        val2 = getValue(mode2, memory[pc + 2])
        if val1 != 0
          pc = val2
        else
          pc += 3
      when 6
        val1 = getValue(mode1, memory[pc + 1])
        val2 = getValue(mode2, memory[pc + 2])
        if val1 == 0
          pc = val2
        else
          pc += 3
      when 7
        val1 = getValue(mode1, memory[pc + 1])
        val2 = getValue(mode2, memory[pc + 2])
        memory[memory[pc + 3]] = if val1 < val2 then 1 else 0
        pc += 4
      when 8
        val1 = getValue(mode1, memory[pc + 1])
        val2 = getValue(mode2, memory[pc + 2])
        memory[memory[pc + 3]] = if val1 == val2 then 1 else 0
        pc += 4
      when 99
        return { output, halted: true, pc, memory }
      else
        throw new Error "Invalid opcode: #{opcode}"

permute = (arr) ->
  if arr.length == 0
    return [[]]
  result = []
  for i in [0...arr.length]
    rest = arr.slice(0, i).concat(arr.slice(i + 1))
    for p in permute(rest)
      result.push [arr[i]].concat(p)
  result

solve = (program) ->
  maxSignal = 0
  for phases in permute([0, 1, 2, 3, 4])
    signal = 0
    for phase in phases
      result = intcode(program, [phase, signal])
      signal = result.output[0]
    maxSignal = Math.max(maxSignal, signal)
  maxSignal

program = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number)
console.log solve(program)

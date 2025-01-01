
fs = require 'fs'

intcode = (program, inputs) ->
  memory = program.slice()
  i = 0
  relativeBase = 0
  outputs = []

  getValue = (mode, pos) ->
    switch mode
      when 0 then memory[memory[pos] or 0] or 0
      when 1 then memory[pos] or 0
      when 2 then memory[memory[pos] + relativeBase or 0] or 0

  setValue = (mode, pos, value) ->
    switch mode
      when 0 then memory[memory[pos] or 0] = value
      when 2 then memory[memory[pos] + relativeBase or 0] = value

  while true
    opcode = memory[i] % 100
    mode1 = Math.floor(memory[i] / 100) % 10
    mode2 = Math.floor(memory[i] / 1000) % 10
    mode3 = Math.floor(memory[i] / 10000) % 10

    switch opcode
      when 1
        val1 = getValue(mode1, i + 1)
        val2 = getValue(mode2, i + 2)
        setValue(mode3, i + 3, val1 + val2)
        i += 4
      when 2
        val1 = getValue(mode1, i + 1)
        val2 = getValue(mode2, i + 2)
        setValue(mode3, i + 3, val1 * val2)
        i += 4
      when 3
        if inputs.length == 0
          return { output: outputs, halted: false, memory: memory, i: i, relativeBase: relativeBase }
        setValue(mode1, i + 1, inputs.shift())
        i += 2
      when 4
        outputs.push(getValue(mode1, i + 1))
        i += 2
      when 5
        if getValue(mode1, i + 1) != 0
          i = getValue(mode2, i + 2)
        else
          i += 3
      when 6
        if getValue(mode1, i + 1) == 0
          i = getValue(mode2, i + 2)
        else
          i += 3
      when 7
        val1 = getValue(mode1, i + 1)
        val2 = getValue(mode2, i + 2)
        setValue(mode3, i + 3, if val1 < val2 then 1 else 0)
        i += 4
      when 8
        val1 = getValue(mode1, i + 1)
        val2 = getValue(mode2, i + 2)
        setValue(mode3, i + 3, if val1 == val2 then 1 else 0)
        i += 4
      when 9
        relativeBase += getValue(mode1, i + 1)
        i += 2
      when 99
        return { output: outputs, halted: true, memory: memory, i: i, relativeBase: relativeBase }
      else
        console.error "Unknown opcode: #{opcode}"
        return { output: outputs, halted: true, memory: memory, i: i, relativeBase: relativeBase }

program = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number)

count = 0
for y in [0...50]
  for x in [0...50]
    result = intcode(program, [x, y]).output[0]
    count += result
    
console.log count

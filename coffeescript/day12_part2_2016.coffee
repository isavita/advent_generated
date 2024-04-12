fs = require 'fs'

getValue = (s, registers) ->
  parseInt(s) or registers[s]

executeInstructions = (instructions, registers) ->
  i = 0
  while i < instructions.length
    parts = instructions[i].split ' '
    switch parts[0]
      when 'cpy'
        val = getValue parts[1], registers
        registers[parts[2]] = val
        i++
      when 'inc'
        registers[parts[1]]++
        i++
      when 'dec'
        registers[parts[1]]--
        i++
      when 'jnz'
        val = getValue parts[1], registers
        if val != 0
          jump = parseInt parts[2]
          i += jump
        else
          i++

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  instructions = data.trim().split '\n'
  registers = {a: 0, b: 0, c: 1, d: 0}
  executeInstructions instructions, registers
  console.log registers['a']
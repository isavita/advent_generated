fs = require 'fs'

data = fs.readFileSync 'input.txt', 'utf8'
instructions = data.trim().split '\n'

registers = a: 1, b: 0

i = 0
while i < instructions.length
  parts = instructions[i].split ' '
  switch parts[0]
    when 'hlf'
      registers[parts[1]] /= 2
    when 'tpl'
      registers[parts[1]] *= 3
    when 'inc'
      registers[parts[1]]++
    when 'jmp'
      offset = parseInt parts[1]
      i += offset - 1
    when 'jie'
      if registers[parts[1][0]] % 2 == 0
        offset = parseInt parts[2]
        i += offset - 1
    when 'jio'
      if registers[parts[1][0]] == 1
        offset = parseInt parts[2]
        i += offset - 1
    else
      throw new Error "Unknown instruction: #{parts[0]}"
  i++

console.log registers['b']
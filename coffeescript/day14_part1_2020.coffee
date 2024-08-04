fs = require 'fs'

input = fs.readFileSync 'input.txt', 'utf8'
lines = input.split '\n'

mask = ''
memory = {}
sum = 0

for line in lines
  if line.startsWith 'mask = '
    mask = line[7..]
  else
    [address, value] = line.match(/mem\[(\d+)\] = (\d+)/)[1..]
    value = parseInt value
    binary = value.toString 2
    binary = '0'.repeat(36) + binary
    binary = binary.slice -36
    result = ''
    for i in [0...36]
      if mask[i] is 'X'
        result += binary[i]
      else
        result += mask[i]
    memory[address] = parseInt result, 2

for value in Object.values memory
  sum += value

console.log sum
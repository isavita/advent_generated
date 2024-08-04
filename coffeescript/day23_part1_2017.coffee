fs = require 'fs'

# Read input from file
input = fs.readFileSync('input.txt', 'utf8').split('\n')

mulCount = 0
pointer = 0
registers = {}

getValue = (s) ->
  if not isNaN(parseInt(s))
    return parseInt(s)
  else
    return registers[s] or 0

while pointer >= 0 and pointer < input.length
  parts = input[pointer].split(/\s+/)
  [cmd, x, y] = parts

  switch cmd
    when 'set'
      registers[x] = getValue(y)
    when 'sub'
      registers[x] = (registers[x] or 0) - getValue(y)
    when 'mul'
      registers[x] = (registers[x] or 0) * getValue(y)
      mulCount++
    when 'jnz'
      if getValue(x) != 0
        pointer += getValue(y) - 1

  pointer++

console.log(mulCount)
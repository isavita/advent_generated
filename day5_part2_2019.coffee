
fs = require 'fs'

programStr = fs.readFileSync('input.txt', 'utf8').trim()
program = programStr.split(',').map (numStr) -> parseInt(numStr)

input = 5
output = 0
i = 0

getValue = (program, pos, mode) ->
  if mode == 0
    return program[program[pos]]
  else
    return program[pos]

while true
  opcode = program[i] % 100
  modes = Math.floor(program[i] / 100)
  param1Mode = modes % 10
  modes = Math.floor(modes / 10)
  param2Mode = modes % 10

  switch opcode
    when 1
      p1 = getValue(program, i + 1, param1Mode)
      p2 = getValue(program, i + 2, param2Mode)
      program[program[i + 3]] = p1 + p2
      i += 4
    when 2
      p1 = getValue(program, i + 1, param1Mode)
      p2 = getValue(program, i + 2, param2Mode)
      program[program[i + 3]] = p1 * p2
      i += 4
    when 3
      program[program[i + 1]] = input
      i += 2
    when 4
      output = getValue(program, i + 1, param1Mode)
      console.log(output)
      i += 2
    when 5
      p1 = getValue(program, i + 1, param1Mode)
      p2 = getValue(program, i + 2, param2Mode)
      if p1 != 0 then i = p2 else i += 3
    when 6
      p1 = getValue(program, i + 1, param1Mode)
      p2 = getValue(program, i + 2, param2Mode)
      if p1 == 0 then i = p2 else i += 3
    when 7
      p1 = getValue(program, i + 1, param1Mode)
      p2 = getValue(program, i + 2, param2Mode)
      program[program[i + 3]] = if p1 < p2 then 1 else 0
      i += 4
    when 8
      p1 = getValue(program, i + 1, param1Mode)
      p2 = getValue(program, i + 2, param2Mode)
      program[program[i + 3]] = if p1 == p2 then 1 else 0
      i += 4
    when 99
      return
    else
      throw new Error("Invalid opcode")

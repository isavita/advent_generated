
fs = require 'fs'

getMode = (instruction, position) ->
  return Math.floor(instruction / Math.pow(10, position + 1)) % 10

getParam = (program, pointer, mode) ->
  if mode == 0
    return program[program[pointer]]
  return program[pointer]

runProgram = (program, input) ->
  output = 0
  pointer = 0
  while pointer < program.length
    instruction = program[pointer]
    opcode = instruction % 100
    switch opcode
      when 1, 2
        param1 = getParam(program, pointer + 1, getMode(instruction, 1))
        param2 = getParam(program, pointer + 2, getMode(instruction, 2))
        result = 0
        if opcode == 1
          result = param1 + param2
        else
          result = param1 * param2
        program[program[pointer + 3]] = result
        pointer += 4
      when 3
        program[program[pointer + 1]] = input
        pointer += 2
      when 4
        output = getParam(program, pointer + 1, getMode(instruction, 1))
        pointer += 2
      when 99
        return output
      else
        throw new Error "Unknown opcode: #{opcode}"

content = fs.readFileSync('input.txt', 'utf8')
program = content.trim().split(',').map((x) -> parseInt(x))
console.log runProgram(program, 1)


fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number)

memory = {}
for i, v of input
  memory[i] = v

runIntcode = (memory) ->
  output = 0
  ip = 0
  relativeBase = 0

  getParam = (offset, modes) ->
    mode = parseInt(modes[modes.length - offset] or 0)
    param = memory[ip + offset] or 0
    switch mode
      when 0 then memory[param] or 0
      when 1 then param
      when 2 then memory[relativeBase + param] or 0
      else throw "unknown parameter mode"

  setParam = (offset, value, modes) ->
    mode = parseInt(modes[modes.length - offset] or 0)
    param = memory[ip + offset] or 0
    switch mode
      when 0 then memory[param] = value
      when 2 then memory[relativeBase + param] = value
      else throw "unknown parameter mode"

  while true
    opcode = memory[ip] % 100
    modes = String(Math.floor(memory[ip] / 100))

    switch opcode
      when 1
        setParam(3, getParam(1, modes) + getParam(2, modes), modes)
        ip += 4
      when 2
        setParam(3, getParam(1, modes) * getParam(2, modes), modes)
        ip += 4
      when 3
        setParam(1, 1, modes)
        ip += 2
      when 4
        output = getParam(1, modes)
        ip += 2
      when 5
        if getParam(1, modes) != 0
          ip = getParam(2, modes)
        else
          ip += 3
      when 6
        if getParam(1, modes) == 0
          ip = getParam(2, modes)
        else
          ip += 3
      when 7
        if getParam(1, modes) < getParam(2, modes)
          setParam(3, 1, modes)
        else
          setParam(3, 0, modes)
        ip += 4
      when 8
        if getParam(1, modes) == getParam(2, modes)
          setParam(3, 1, modes)
        else
          setParam(3, 0, modes)
        ip += 4
      when 9
        relativeBase += getParam(1, modes)
        ip += 2
      when 99
        return output
      else
        throw "unknown opcode: #{opcode}"

console.log runIntcode(memory)

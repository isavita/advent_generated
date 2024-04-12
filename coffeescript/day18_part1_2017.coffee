fs = require 'fs'

getValue = (arg, registers) ->
  parseInt(arg) or registers[arg] or 0

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  instructions = (line.split(/\s+/) for line in data.trim().split('\n'))
  registers = {}
  lastSound = null
  i = 0

  while i < instructions.length
    [cmd, arg1, arg2] = instructions[i]

    switch cmd
      when "snd"
        lastSound = getValue(arg1, registers)
      when "set"
        registers[arg1] = getValue(arg2, registers)
      when "add"
        registers[arg1] += getValue(arg2, registers)
      when "mul"
        registers[arg1] *= getValue(arg2, registers)
      when "mod"
        registers[arg1] %= getValue(arg2, registers)
      when "rcv"
        if getValue(arg1, registers) != 0
          console.log lastSound
          return
      when "jgz"
        if getValue(arg1, registers) > 0
          i += getValue(arg2, registers)
          continue

    i++
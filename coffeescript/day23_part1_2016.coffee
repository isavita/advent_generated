
fs = require 'fs'

readInstructions = (filename) ->
  fs.readFileSync(filename, 'utf-8').trim().split('\n')

getValue = (s, registers) ->
  if /^-?\d+$/.test s
    parseInt s
  else
    registers[s]

toggleInstruction = (instr) ->
  parts = instr.split ' '
  switch parts[0]
    when 'inc' then parts[0] = 'dec'
    when 'dec', 'tgl' then parts[0] = 'inc'
    when 'jnz' then parts[0] = 'cpy'
    when 'cpy' then parts[0] = 'jnz'
  parts.join ' '

executeInstructions = (instructions, registers) ->
  pc = 0
  while pc < instructions.length
    fields = instructions[pc].split ' '
    switch fields[0]
      when 'cpy'
        x = getValue fields[1], registers
        if registers[fields[2]]?
          registers[fields[2]] = x
      when 'inc'
        if registers[fields[1]]?
          registers[fields[1]]++
      when 'dec'
        if registers[fields[1]]?
          registers[fields[1]]--
      when 'jnz'
        x = getValue fields[1], registers
        if x != 0
          pc += getValue(fields[2], registers) - 1
      when 'tgl'
        x = getValue fields[1], registers
        tgt = pc + x
        if tgt >= 0 and tgt < instructions.length
          instructions[tgt] = toggleInstruction instructions[tgt]
    pc++

instructions = readInstructions 'input.txt'
registers = {a: 7, b: 0, c: 0, d: 0}
executeInstructions instructions, registers
console.log registers.a

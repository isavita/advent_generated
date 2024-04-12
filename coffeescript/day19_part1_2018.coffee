fs = require 'fs'

toInt = (s) -> parseInt(s, 10)
boolToInt = (b) -> if b then 1 else 0

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  lines = data.trim().split '\n'
  ipBind = toInt(lines[0].split(' ')[1])
  instructions = (line.split(' ') for line in lines[1..])

  registers = [0, 0, 0, 0, 0, 0]
  ip = 0

  while ip < instructions.length
    registers[ipBind] = ip
    [opcode, a, b, c] = instructions[ip]
    a = toInt(a)
    b = toInt(b)
    c = toInt(c)

    switch opcode
      when 'addr' then registers[c] = registers[a] + registers[b]
      when 'addi' then registers[c] = registers[a] + b
      when 'mulr' then registers[c] = registers[a] * registers[b]
      when 'muli' then registers[c] = registers[a] * b
      when 'banr' then registers[c] = registers[a] & registers[b]
      when 'bani' then registers[c] = registers[a] & b
      when 'borr' then registers[c] = registers[a] | registers[b]
      when 'bori' then registers[c] = registers[a] | b
      when 'setr' then registers[c] = registers[a]
      when 'seti' then registers[c] = a
      when 'gtir' then registers[c] = boolToInt(a > registers[b])
      when 'gtri' then registers[c] = boolToInt(registers[a] > b)
      when 'gtrr' then registers[c] = boolToInt(registers[a] > registers[b])
      when 'eqir' then registers[c] = boolToInt(a == registers[b])
      when 'eqri' then registers[c] = boolToInt(registers[a] == b)
      when 'eqrr' then registers[c] = boolToInt(registers[a] == registers[b])

    ip = registers[ipBind]
    ip += 1

    if ip < 0 or ip >= instructions.length
      break

  console.log registers[0]
fs = require 'fs'

class OP
  constructor: (@action, @a, @b, @name, @matchCount = []) ->

strToInt = (s) -> parseInt s, 10

regSplit = (text, delimiter) ->
  regex = new RegExp delimiter, 'g'
  result = []
  lastStart = 0
  while match = regex.exec text
    result.push text[lastStart...match.index]
    lastStart = match.index + match[0].length
  result.push text[lastStart...]
  result

runOp = (op, registers, instruction) ->
  registerCP = registers.slice()
  A = if op.a == 'r' then registerCP[instruction[1]] else instruction[1]
  B = if op.b == 'r' then registerCP[instruction[2]] else instruction[2]
  switch op.action
    when '+'
      registerCP[instruction[3]] = A + B
    when '*'
      registerCP[instruction[3]] = A * B
    when '&'
      registerCP[instruction[3]] = A & B
    when '|'
      registerCP[instruction[3]] = A | B
    when 'a'
      registerCP[instruction[3]] = A
    when '>'
      registerCP[instruction[3]] = if A > B then 1 else 0
    when '='
      registerCP[instruction[3]] = if A == B then 1 else 0
  registerCP

match = (r, c) ->
  return false if r.length isnt c.length
  for i in [0...r.length]
    return false if r[i] isnt c[i]
  true

add = (op, c) ->
  op.matchCount.push c unless c in op.matchCount

testCode = (registers, n, instruction, opcodes) ->
  sum = 0
  for op in opcodes
    if match(n, runOp(op, registers, instruction))
      add op, instruction[0]
      sum++
  sum

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  lines = data.trim().split '\n'
  opcodes = [
    new OP('+', 'r', 'r', 'addr'),
    new OP('+', 'r', 'v', 'addi'),
    new OP('*', 'r', 'r', 'mulr'),
    new OP('*', 'r', 'v', 'muli'),
    new OP('&', 'r', 'r', 'banr'),
    new OP('&', 'r', 'v', 'bani'),
    new OP('|', 'r', 'r', 'borr'),
    new OP('|', 'r', 'v', 'bori'),
    new OP('a', 'r', 'r', 'setr'),
    new OP('a', 'v', 'r', 'seti'),
    new OP('>', 'v', 'r', 'gtir'),
    new OP('>', 'r', 'v', 'gtri'),
    new OP('>', 'r', 'r', 'gtrr'),
    new OP('=', 'v', 'r', 'eqir'),
    new OP('=', 'r', 'v', 'eqri'),
    new OP('=', 'r', 'r', 'eqrr')
  ]

  sum = 0
  lineCount = 0
  while lineCount < lines.length
    if lines[lineCount][0] == 'B'
      split = regSplit(lines[lineCount], "[^0-9]+")
      registers = [strToInt(split[1]), strToInt(split[2]), strToInt(split[3]), strToInt(split[4])]
      split = regSplit(lines[lineCount+1], "[^0-9]+")
      instruction = [strToInt(split[0]), strToInt(split[1]), strToInt(split[2]), strToInt(split[3])]
      split = regSplit(lines[lineCount+2], "[^0-9]+")
      n = [strToInt(split[1]), strToInt(split[2]), strToInt(split[3]), strToInt(split[4])]
      tempSum = testCode(registers, n, instruction, opcodes)
      sum++ if tempSum >= 3
      lineCount += 4
    else
      break

  console.log sum
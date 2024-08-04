fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

opcodes = [
  {name: 'addr', action: '+', a: 'r', b: 'r', matchCount: []},
  {name: 'addi', action: '+', a: 'r', b: 'v', matchCount: []},
  {name: 'mulr', action: '*', a: 'r', b: 'r', matchCount: []},
  {name: 'muli', action: '*', a: 'r', b: 'v', matchCount: []},
  {name: 'banr', action: '&', a: 'r', b: 'r', matchCount: []},
  {name: 'bani', action: '&', a: 'r', b: 'v', matchCount: []},
  {name: 'borr', action: '|', a: 'r', b: 'r', matchCount: []},
  {name: 'bori', action: '|', a: 'r', b: 'v', matchCount: []},
  {name: 'setr', action: 'a', a: 'r', b: 'r', matchCount: []},
  {name: 'seti', action: 'a', a: 'v', b: 'r', matchCount: []},
  {name: 'gtir', action: '>', a: 'v', b: 'r', matchCount: []},
  {name: 'gtri', action: '>', a: 'r', b: 'v', matchCount: []},
  {name: 'gtrr', action: '>', a: 'r', b: 'r', matchCount: []},
  {name: 'eqir', action: '=', a: 'v', b: 'r', matchCount: []},
  {name: 'eqri', action: '=', a: 'r', b: 'v', matchCount: []},
  {name: 'eqrr', action: '=', a: 'r', b: 'r', matchCount: []}
]

sum = 0
lineCount = 0

testCode = (registers, result, instruction, opcodes) ->
  sum = 0
  for op in opcodes
    if match(result, runOp(op, registers, instruction))
      add(op, instruction[0])
      sum++
  sum

match = (r, c) ->
  return false unless r.length is c.length
  for i in [0...r.length]
    return false unless r[i] is c[i]
  true

remove = (op, c) ->
  op.matchCount = op.matchCount.filter (v) -> v != c

add = (op, c) ->
  op.matchCount.push(c) unless c in op.matchCount

runOp = (op, registers, instruction) ->
  registerCP = registers.slice()
  A = if op.a is 'r' then registerCP[instruction[1]] else instruction[1]
  B = if op.b is 'r' then registerCP[instruction[2]] else instruction[2]
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
      registerCP[instruction[3]] = if A is B then 1 else 0
  registerCP

while lineCount < input.length
  if input[lineCount][0] is 'B'
    registers = input[lineCount + 0].match(/\d+/g).map(Number)
    instruction = input[lineCount + 1].match(/\d+/g).map(Number)
    result = input[lineCount + 2].match(/\d+/g).map(Number)
    tempSum = testCode(registers, result, instruction, opcodes)

    sum++ if tempSum >= 3
    lineCount += 4
  else
    break

orderedOpCodes = {}

while Object.keys(orderedOpCodes).length < 16
  for op in opcodes
    if op.matchCount?.length is 1
      c = op.matchCount[0]
      orderedOpCodes[c] = op
      for o in opcodes
        remove(o, c)

lineCount += 2
r = [0, 0, 0, 0]

while lineCount < input.length
  instruction = input[lineCount].match(/\d+/g).map(Number)
  r = runOp orderedOpCodes[instruction[0]], r, instruction
  lineCount++

console.log r[0]
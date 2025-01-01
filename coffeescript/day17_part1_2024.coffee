
fs = require 'fs'

data = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

A = 0
B = 0
C = 0
program = []

for line in data
  line = line.trim()
  continue if not line
  
  if line.startsWith('Register A:')
    A = parseInt(line.split(':')[1].trim())
  else if line.startsWith('Register B:')
    B = parseInt(line.split(':')[1].trim())
  else if line.startsWith('Register C:')
    C = parseInt(line.split(':')[1].trim())
  else if line.startsWith('Program:')
    program = (parseInt(num.trim()) for num in line.split(':')[1].trim().split(','))

getComboVal = (op) ->
  switch
    when op <= 3 then op
    when op == 4 then A
    when op == 5 then B
    when op == 6 then C
    else throw new Error("invalid combo operand")

outputVals = []
ip = 0

while ip < program.length
  opcode = program[ip]
  break if ip + 1 >= program.length
  operand = program[ip + 1]

  switch opcode
    when 0 # adv
      den = getComboVal(operand)
      A = if den == 0 then 0 else Math.floor(A / (2 ** den))
      ip += 2
    when 1 # bxl
      B ^= operand
      ip += 2
    when 2 # bst
      B = getComboVal(operand) % 8
      ip += 2
    when 3 # jnz
      if A != 0
        ip = operand
      else
        ip += 2
    when 4 # bxc
      B ^= C
      ip += 2
    when 5 # out
      outputVals.push(String(getComboVal(operand) % 8))
      ip += 2
    when 6 # bdv
      den = getComboVal(operand)
      B = Math.floor(A / (2 ** den))
      ip += 2
    when 7 # cdv
      den = getComboVal(operand)
      C = Math.floor(A / (2 ** den))
      ip += 2
    else
      break

console.log outputVals.join(',')

fs = require 'fs'

# Read input from file
input = fs.readFileSync 'input.txt', 'utf8'

# Initialize registers
registers = {}
highestValue = 0

# Process instructions
for line in input.trim().split '\n'
  [reg, op, amount, , condReg, condOp, condVal] = line.split ' '
  amount = parseInt amount
  condVal = parseInt condVal

  # Default register values to 0 if not present
  registers[reg] ?= 0
  registers[condReg] ?= 0

  # Evaluate condition
  cond = false
  switch condOp
    when '>' then cond = registers[condReg] > condVal
    when '>=' then cond = registers[condReg] >= condVal
    when '<' then cond = registers[condReg] < condVal
    when '<=' then cond = registers[condReg] <= condVal
    when '==' then cond = registers[condReg] == condVal
    when '!=' then cond = registers[condReg] != condVal

  if cond
    switch op
      when 'inc' then registers[reg] += amount
      when 'dec' then registers[reg] -= amount

    # Update highest value
    if registers[reg] > highestValue
      highestValue = registers[reg]

# Print the highest value
console.log highestValue
fs = require 'fs'

# Read Input
input = fs.readFileSync 'input.txt', 'utf8'

# Initialize Registers
registers = {}

# Process Instructions
for line in input.trim().split '\n'
    parts = line.split ' '
    reg = parts[0]
    op = parts[1]
    amount = parseInt parts[2]
    condReg = parts[4]
    condOp = parts[5]
    condVal = parseInt parts[6]

    # Check condition
    cond = false
    switch condOp
        when '>' then cond = (registers[condReg] or 0) > condVal
        when '>=' then cond = (registers[condReg] or 0) >= condVal
        when '<' then cond = (registers[condReg] or 0) < condVal
        when '<=' then cond = (registers[condReg] or 0) <= condVal
        when '==' then cond = (registers[condReg] or 0) == condVal
        when '!=' then cond = (registers[condReg] or 0) != condVal

    if cond
        switch op
            when 'inc' then registers[reg] = (registers[reg] or 0) + amount
            when 'dec' then registers[reg] = (registers[reg] or 0) - amount

# Find Max Value
maxValue = Math.max (registers[key] for key of registers)...

console.log maxValue
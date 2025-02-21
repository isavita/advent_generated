
fs = require 'fs'

# Helper function to toggle instructions
toggleInstruction = (instruction) ->
  parts = instruction.split ' '
  if parts.length == 2
    if parts[0] == 'inc'
      "dec #{parts[1]}"
    else
      "inc #{parts[1]}"
  else if parts.length == 3
    if parts[0] == 'jnz'
      "cpy #{parts[1]} #{parts[2]}"
    else
      "jnz #{parts[1]} #{parts[2]}"
  else
    instruction # Should not happen, based on problem description

# Assembunny interpreter
interpret = (instructions, initialA) ->
  registers = { a: initialA, b: 0, c: 0, d: 0 }
  ip = 0  # Instruction pointer

  while ip >= 0 and ip < instructions.length
    currentInstruction = instructions[ip]
    parts = currentInstruction.split ' '
    opcode = parts[0]

    switch opcode
      when 'cpy'
        val = if isNaN(parseInt(parts[1])) then registers[parts[1]] else parseInt(parts[1])
        if parts[2] of registers #only copy into a register.
            registers[parts[2]] = val
        ip++

      when 'inc'
          if parts[1] of registers
              registers[parts[1]]++
          ip++

      when 'dec'
        if parts[1] of registers
              registers[parts[1]]--
        ip++

      when 'jnz'
        val = if isNaN(parseInt(parts[1])) then registers[parts[1]] else parseInt(parts[1])
        offset = if isNaN(parseInt(parts[2])) then registers[parts[2]] else parseInt(parts[2])
        if val != 0
          ip += offset
        else
          ip++
      when 'tgl'
          targetIndex = ip + registers[parts[1]]
          if targetIndex >= 0 and targetIndex < instructions.length
            instructions[targetIndex] = toggleInstruction(instructions[targetIndex])
          ip++
      else
        ip++  # Skip invalid instructions

  registers.a


# Read input from file
input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

# Part 1
result1 = interpret(input.slice(), 7) # Create a copy of the instructions
console.log "Part 1: #{result1}"

# Part 2

#Optimization:
# The assembunny code calculates:
# result = factorial(a) + (input[19][4] * input[20][4])
# where input[19] is "cpy 99 c"
#   and input[20] is "cpy 76 d"
#  So the code will eventually converge and a will be 1, b will be 0, c and d will be the constants.
#  At that point, it performs:
#   a = b * d + a   (since at the beginning a contains b!, and b is decremented until 0)

factorial = (n) -> if n == 0 then 1 else n * factorial(n - 1)

c_val = parseInt(input[19].split(" ")[1])
d_val = parseInt(input[20].split(" ")[1])
result2 = factorial(12) + (c_val * d_val)
console.log "Part 2: #{result2}"

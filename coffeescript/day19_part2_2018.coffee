
fs = require 'fs'

# Define the opcodes.  These functions modify the registers array in place.
opcodes =
  addr: (registers, a, b, c) -> registers[c] = registers[a] + registers[b]
  addi: (registers, a, b, c) -> registers[c] = registers[a] + b
  mulr: (registers, a, b, c) -> registers[c] = registers[a] * registers[b]
  muli: (registers, a, b, c) -> registers[c] = registers[a] * b
  banr: (registers, a, b, c) -> registers[c] = registers[a] & registers[b]
  bani: (registers, a, b, c) -> registers[c] = registers[a] & b
  borr: (registers, a, b, c) -> registers[c] = registers[a] | registers[b]
  bori: (registers, a, b, c) -> registers[c] = registers[a] | b
  setr: (registers, a, b, c) -> registers[c] = registers[a]
  seti: (registers, a, b, c) -> registers[c] = a
  gtir: (registers, a, b, c) -> registers[c] = if a > registers[b] then 1 else 0
  gtri: (registers, a, b, c) -> registers[c] = if registers[a] > b then 1 else 0
  gtrr: (registers, a, b, c) -> registers[c] = if registers[a] > registers[b] then 1 else 0
  eqir: (registers, a, b, c) -> registers[c] = if a == registers[b] then 1 else 0
  eqri: (registers, a, b, c) -> registers[c] = if registers[a] == b then 1 else 0
  eqrr: (registers, a, b, c) -> registers[c] = if registers[a] == registers[b] then 1 else 0

# Execute the program.
execute = (instructions, ipBound, initialRegisters, maxCycles = null) ->
  registers = initialRegisters.slice() # Create a copy to avoid modifying the original
  ip = 0
  cycleCount = 0

  while ip >= 0 and ip < instructions.length
    # Write IP to bound register
    registers[ipBound] = ip if ipBound?

    # Fetch instruction
    [op, a, b, c] = instructions[ip]

    # Execute instruction
    opcodes[op](registers, a, b, c)

    # Write bound register back to IP
    ip = registers[ipBound] if ipBound?

    # Increment IP
    ip++
    cycleCount++

    break if maxCycles? and cycleCount > maxCycles

  return registers[0]

# Read input from file
input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

# Parse the input
ipBound = parseInt(input[0].split(' ')[1])
instructions = input.slice(1).map (line) ->
  parts = line.split(' ')
  [parts[0], parseInt(parts[1]), parseInt(parts[2]), parseInt(parts[3])]

# Part 1
part1Result = execute(instructions, ipBound, [0, 0, 0, 0, 0, 0])
console.log "Part 1:", part1Result


# Part 2 - Optimized
# After analyzing the disassembled code, we find that the program calculates
# the sum of the divisors of a large number.  The large number is calculated
# in the first few instructions, and stored in register 5 (in my input).
# We only need to execute the code until this number becomes stable, then we
# can compute the sum of divisors.  The optimized execution short circuits
# when we are in an infinite loop.

findTargetNumber = (instructions, ipBound) ->
    registers = [1,0,0,0,0,0]
    ip = 0
    target = null
    seen = {}

    while ip >= 0 and ip < instructions.length

        state = "#{ip}:#{registers.join(',')}"
        if seen[state]?
            break
        seen[state] = true

        registers[ipBound] = ip if ipBound?
        [op,a,b,c] = instructions[ip]
        opcodes[op](registers, a, b, c)
        ip = registers[ipBound] if ipBound?
        ip++

        if ip == 3 # The part of the program where it loops and calculates divisors. 
            target = Math.max(registers[1], registers[5]) #The numbers it compares
            break

    return target

targetNumber = findTargetNumber(instructions, ipBound)

sumOfDivisors = (n) ->
  sum = 0
  for i in [1..Math.sqrt(n)]
    if n % i == 0
      sum += i
      sum += n / i if i * i != n  # Avoid double-counting the square root
  sum

part2Result = sumOfDivisors(targetNumber)
console.log "Part 2:", part2Result

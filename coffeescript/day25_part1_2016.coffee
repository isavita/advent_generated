fs = require 'fs'

# Function to execute the assembunny code
executeAssembunny = (instructions, initialValue) ->
  registers = {a: initialValue, b: 0, c: 0, d: 0}
  lastOutput = null
  outputCount = 0
  i = 0

  while i < instructions.length and outputCount < 1000 # Limit iterations to prevent infinite loop
    parts = instructions[i].split(' ')
    instr = parts[0]
    x = parts[1]
    y = parts[2] if parts.length == 3

    switch instr
      when 'cpy'
        registers[y] = if isNaN(parseInt(x)) then registers[x] else parseInt(x)
      when 'inc'
        registers[x]++
      when 'dec'
        registers[x]--
      when 'jnz'
        xValue = if isNaN(parseInt(x)) then registers[x] else parseInt(x)
        if xValue != 0
          i += parseInt(y) - 1
      when 'out'
        xValue = if isNaN(parseInt(x)) then registers[x] else parseInt(x)
        if lastOutput != null and lastOutput == xValue
          return false # Signal does not alternate
        lastOutput = xValue
        outputCount++
        if outputCount >= 1000 # Arbitrary large number for checking pattern
          return true # Assuming pattern holds
    i++
  false

# Function to find the initial value that produces the alternating signal
findInitialValue = (instructions) ->
  initialValue = 0
  while true
    if executeAssembunny(instructions, initialValue)
      return initialValue
    initialValue++

# Read the assembunny code from the input file and solve the puzzle
fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log err
    return
  instructions = data.trim().split('\n')
  initialValue = findInitialValue(instructions)
  console.log initialValue

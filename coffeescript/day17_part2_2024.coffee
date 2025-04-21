
fs = require 'fs'

computeOperand = (val, a, b, c) ->
  switch val
    when 0, 1, 2, 3 then val
    when 4 then a
    when 5 then b
    when 6 then c
    else throw new Error "Invalid combo operand: #{val}"

simulateComputer = (programObj) ->
  outs = []
  # Use BigInt for registers to handle potential overflows with shifts, mimicking Go's int64
  a = BigInt(programObj.a)
  b = BigInt(programObj.b)
  c = BigInt(programObj.c)
  input = programObj.program
  i = 0

  # Use a while loop for clearer jump logic (opcode 3)
  while i < input.length
    cmd = input[i]
    operandValRaw = input[i + 1] # Keep raw value for opcode 1

    # Need to compute operand based on current a, b, c for most opcodes
    operandComputed = -> BigInt(computeOperand(operandValRaw, a, b, c))

    switch cmd
      when 0
        a >>= operandComputed()
      when 1
        # Opcode 1 uses the immediate value directly
        b ^= BigInt(operandValRaw)
      when 2
        b = operandComputed() % 8n # Modulo 8
      when 3
        if a != 0n
          # Jump: set i to the target instruction index for the next iteration
          i = operandValRaw
          continue # Skip the default i += 2 increment
        # else: fall through to next instruction
      when 4
        b ^= c
      when 5
        # Output is operand % 8. Convert result back to Number for the outs array.
        outs.push Number(operandComputed() % 8n)
      when 6
        b = a >> operandComputed()
      when 7
        c = a >> operandComputed()
      else
        throw new Error "Invalid opcode: #{cmd}"

    i += 2 # Move to the next instruction pair

  return outs

check = (p) ->
  program = p.program
  valids = []
  # Use BigInt for score as it can grow large (8^depth)
  stack = [{depth: 0, score: 0n}]
  # Use Set with string keys for efficient visited state tracking
  seen = new Set()

  targetLength = program.length # Termination depth from Go code

  while stack.length > 0
    state = stack.pop()
    stateKey = "#{state.depth},#{state.score}"

    continue if seen.has(stateKey)
    seen.add(stateKey)

    depth = state.depth
    score = state.score # This is a BigInt

    if depth == targetLength
      valids.push score # Store the BigInt score
    else if depth < targetLength
      # Check index boundary before accessing program
      comparisonIndex = program.length - 1 - depth
      if comparisonIndex < 0
          # Should not happen if depth < targetLength but good practice
          continue

      # The value to compare against comes directly from the program array (as in Go)
      targetComparisonValue = BigInt(program[comparisonIndex])

      # Iterate through possible next 'digits' (0-7) for constructing 'a' (score)
      for i in [0...8]
        digit = BigInt(i)
        # Construct the next potential 'a' value (score) in base 8
        # newScore = i + 8 * score
        newScore = digit + 8n * score

        # Create a test program configuration with the new potential 'a'
        testProgram = { a: newScore, b: p.b, c: p.c, program: program }
        result = simulateComputer(testProgram)

        # Check if the *first* output matches the target comparison value for this depth
        if result.length > 0 and BigInt(result[0]) == targetComparisonValue
          stack.push { depth: depth + 1, score: newScore }

  return valids

main = ->
  try
    input = fs.readFileSync('input.txt', 'utf8')
    lines = input.split('\n')

    a_init = 0n # Use BigInt from the start
    b_init = 0n
    c_init = 0n
    program = []

    for line in lines
      line = line.trim()
      if line.startsWith "Register A:"
        # Initial 'a' isn't actually used by check, but parse anyway
        a_init = BigInt(line.split(":")[1].trim())
      else if line.startsWith "Register B:"
        b_init = BigInt(line.split(":")[1].trim())
      else if line.startsWith "Register C:"
        c_init = BigInt(line.split(":")[1].trim())
      else if line.startsWith "Program:"
        nums = line.split(":")[1].trim().split(',')
        program = (parseInt(n.trim(), 10) for n in nums) # Program instructions are Numbers

    # The check function only needs initial b, c and the program code.
    # The 'a' value is what 'check' is trying to find (represented by 'score').
    p = { a: 0n, b: b_init, c: c_init, program: program }

    validValues = check(p) # Returns array of BigInts

    if validValues.length == 0
        console.log "No valid 'a' values found."
        return

    # Find the minimum valid value (BigInt comparison)
    minVal = validValues[0]
    for val in validValues[1...]
      if val < minVal
        minVal = val

    # Convert BigInt to string for printing
    console.log minVal.toString()

  catch err
    console.error "Error:", err
    process.exit(1)

main()

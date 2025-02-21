
fs = require 'fs'

# Intcode computer
intcode = (program, inputs, pointer = 0) ->
  output = []
  relativeBase = 0

  getParameter = (mode, offset) ->
    address = switch mode
      when 0 then program[pointer + offset]
      when 1 then pointer + offset
      when 2 then program[pointer + offset] + relativeBase
      else throw new Error "Invalid mode: #{mode}"

    if address < 0
      throw new Error "Invalid address: #{address}"

    program[address] ? 0

  setParameter = (mode, offset, value) ->
    address = switch mode
      when 0 then program[pointer + offset]
      when 2 then program[pointer + offset] + relativeBase
      else throw new Error "Invalid mode for setting parameter: #{mode}"

    if address < 0
      throw new Error "Invalid address: #{address}"

    program[address] = value

  run = () ->
    while true
      instruction = program[pointer] % 100
      mode1 = Math.floor(program[pointer] / 100) % 10
      mode2 = Math.floor(program[pointer] / 1000) % 10
      mode3 = Math.floor(program[pointer] / 10000) % 10

      switch instruction
        when 1
          param1 = getParameter(mode1, 1)
          param2 = getParameter(mode2, 2)
          setParameter(mode3, 3, param1 + param2)
          pointer += 4
        when 2
          param1 = getParameter(mode1, 1)
          param2 = getParameter(mode2, 2)
          setParameter(mode3, 3, param1 * param2)
          pointer += 4
        when 3
          if inputs.length == 0
            return { program, output, pointer, halted: false }
          value = inputs.shift()
          setParameter(mode1, 1, value)
          pointer += 2
        when 4
          output.push getParameter(mode1, 1)
          pointer += 2
        when 5
          param1 = getParameter(mode1, 1)
          param2 = getParameter(mode2, 2)
          if param1 != 0
            pointer = param2
          else
            pointer += 3
        when 6
          param1 = getParameter(mode1, 1)
          param2 = getParameter(mode2, 2)
          if param1 == 0
            pointer = param2
          else
            pointer += 3
        when 7
          param1 = getParameter(mode1, 1)
          param2 = getParameter(mode2, 2)
          setParameter(mode3, 3, if param1 < param2 then 1 else 0)
          pointer += 4
        when 8
          param1 = getParameter(mode1, 1)
          param2 = getParameter(mode2, 2)
          setParameter(mode3, 3, if param1 == param2 then 1 else 0)
          pointer += 4
        when 9
          param1 = getParameter(mode1, 1)
          relativeBase += param1
          pointer += 2
        when 99
          return { program, output, pointer, halted: true }
        else
          throw new Error "Unknown opcode: #{instruction} at position #{pointer}"
  
  run()

# Permutation function
permutations = (arr) ->
  if arr.length <= 1
    return [arr]

  result = []
  for i in [0...arr.length]
    first = arr[i]
    rest = arr.slice(0, i).concat(arr.slice(i + 1))
    for p in permutations(rest)
      result.push([first].concat(p))

  return result

# Part 1
part1 = (program) ->
  phaseSettings = [0, 1, 2, 3, 4]
  maxSignal = 0

  for phases in permutations(phaseSettings)
    signal = 0
    for phase in phases
      result = intcode(program.slice(), [phase, signal])
      signal = result.output[0]
    maxSignal = Math.max(maxSignal, signal)

  return maxSignal

# Part 2
part2 = (program) ->
  phaseSettings = [5, 6, 7, 8, 9]
  maxSignal = 0

  for phases in permutations(phaseSettings)
    amplifiers = []
    for i in [0..4]
      amplifiers.push({
        program: program.slice(),
        inputs: [phases[i]],
        pointer: 0,
        halted: false
      })

    signal = 0
    ampIndex = 0
    while !amplifiers.every((amp) -> amp.halted)
      amplifiers[ampIndex].inputs.push(signal)
      result = intcode(amplifiers[ampIndex].program, amplifiers[ampIndex].inputs, amplifiers[ampIndex].pointer)
      amplifiers[ampIndex].program = result.program
      amplifiers[ampIndex].pointer = result.pointer
      amplifiers[ampIndex].halted = result.halted
      signal = result.output[0]
      amplifiers[ampIndex].output = [] 

      ampIndex = (ampIndex + 1) % 5

      if amplifiers.every((amp) -> amp.halted)
        maxSignal = Math.max(maxSignal, signal)

  return maxSignal

# Main
main = () ->
  programText = fs.readFileSync('input.txt', 'utf-8').trim()
  program = programText.split(',').map((x) -> parseInt(x))
  
  console.log "Part 1: #{part1(program)}"
  console.log "Part 2: #{part2(program)}"

main()

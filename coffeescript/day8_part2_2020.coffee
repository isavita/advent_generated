fs = require 'fs'

parseInstruction = (instruction) ->
  [op, arg] = instruction.split ' '
  [op, parseInt(arg)]

executeBootCode = (instructions) ->
  accumulator = 0
  visited = {}
  currentInstruction = 0

  while currentInstruction < instructions.length
    return [accumulator, false] if visited[currentInstruction]
    visited[currentInstruction] = true

    [op, arg] = parseInstruction instructions[currentInstruction]
    switch op
      when 'acc'
        accumulator += arg
        currentInstruction++
      when 'jmp'
        currentInstruction += arg
      when 'nop'
        currentInstruction++

  [accumulator, true]

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error opening file:", err
    return

  instructions = data.trim().split '\n'
  
  for i in [0...instructions.length]
    [op, arg] = parseInstruction instructions[i]
    continue if op == 'acc'

    modifiedInstructions = instructions.slice()
    modifiedInstructions[i] = if op == 'jmp' then "nop #{arg}" else "jmp #{arg}"

    [accumulator, terminated] = executeBootCode modifiedInstructions
    if terminated
      console.log accumulator
      break
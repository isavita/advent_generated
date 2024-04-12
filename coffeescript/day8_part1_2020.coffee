fs = require 'fs'

executeBootCode = (instructions) ->
  accumulator = 0
  visited = {}
  currentInstruction = 0

  while currentInstruction < instructions.length
    if visited[currentInstruction]
      return accumulator

    visited[currentInstruction] = true
    parts = instructions[currentInstruction].split ' '
    op = parts[0]
    arg = parseInt parts[1]

    switch op
      when 'acc'
        accumulator += arg
        currentInstruction++
      when 'jmp'
        currentInstruction += arg
      when 'nop'
        currentInstruction++

  accumulator

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error opening file:", err
    return

  instructions = data.trim().split '\n'
  accumulator = executeBootCode instructions
  console.log accumulator
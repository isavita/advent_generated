
fs = require 'fs'

class IntcodeComputer
  constructor: (@memory, @inputs = [], @outputs = []) ->
    @ip = 0
    @relativeBase = 0
    @halted = false

  run: ->
    while !@halted
      opcode = @memory[@ip] % 100
      modes = Math.floor(@memory[@ip] / 100)

      param = (offset) =>
        mode = Math.floor(modes / (10 ** (offset - 1))) % 10
        switch mode
          when 0 then @memory[@ip + offset] || 0
          when 1 then @ip + offset
          when 2 then @memory[@ip + offset] + @relativeBase || 0

      write = (offset, value) =>
        mode = Math.floor(modes / (10 ** (offset - 1))) % 10
          
        address = switch mode
            when 0 then @memory[@ip + offset] || 0
            when 2 then @memory[@ip+offset] + @relativeBase || 0
            else throw new Error("Invalid write mode")
          
        if address >= @memory.length
            @memory.length = address + 1
        @memory[address] = value
      
      switch opcode
        when 1
          write(3, (@memory[param(1)] || 0) + (@memory[param(2)] || 0))
          @ip += 4
        when 2
          write(3, (@memory[param(1)] || 0) * (@memory[param(2)] || 0))
          @ip += 4
        when 3
          if @inputs.length == 0
            return  # Pause execution if no input
          write(1, @inputs.shift())
          @ip += 2
        when 4
          @outputs.push(@memory[param(1)] || 0)
          @ip += 2
        when 5
          if (@memory[param(1)] || 0) != 0
            @ip = (@memory[param(2)] || 0)
          else
            @ip += 3
        when 6
          if (@memory[param(1)] || 0) == 0
            @ip = (@memory[param(2)] || 0)
          else
            @ip += 3
        when 7
            write(3, if (@memory[param(1)] || 0) < (@memory[param(2)] || 0) then 1 else 0)
            @ip += 4
        when 8
            write(3, if (@memory[param(1)] || 0) == (@memory[param(2)] || 0) then 1 else 0)
            @ip += 4
        when 9
            @relativeBase += (@memory[param(1)] || 0)
            @ip += 2
        when 99
          @halted = true
        else
          throw new Error("Unknown opcode: #{opcode}")

    return @outputs


solvePart1 = (program) ->
  computer = new IntcodeComputer(program.slice())
  computer.run()
  outputs = computer.outputs
  blockCount = 0
  for i in [0...outputs.length] by 3
    if outputs[i+2] == 2
      blockCount++
  blockCount

solvePart2 = (program) ->
  program[0] = 2 # Play for free
  computer = new IntcodeComputer(program.slice())
  score = 0
  ballX = 0
  paddleX = 0
  
  while !computer.halted
    computer.run()
    outputs = computer.outputs
    computer.outputs = []

    for i in [0...outputs.length] by 3
      x = outputs[i]
      y = outputs[i+1]
      tileId = outputs[i+2]

      if x == -1 and y == 0
        score = tileId
      else if tileId == 4
        ballX = x
      else if tileId == 3
        paddleX = x

    joystickInput = 0
    if ballX < paddleX
        joystickInput = -1
    else if ballX > paddleX
        joystickInput = 1

    computer.inputs.push(joystickInput)


  score



input = fs.readFileSync('input.txt', 'utf8').trim()
program = input.split(',').map(Number)

part1Result = solvePart1(program)
console.log "Part 1:", part1Result

part2Result = solvePart2(program)
console.log "Part 2:", part2Result


fs = require 'fs'

class IntcodeComputer
  constructor: (@program) ->
    @memory = @program.slice()
    @instructionPointer = 0
    @relativeBase = 0
    @inputBuffer = []
    @outputBuffer = []
    @halted = false

  getMemory: (address) ->
    if address < 0
      throw new Error("Invalid memory address: #{address}")
    if address >= @memory.length
      @memory[address] = 0
    @memory[address]

  setMemory: (address, value) ->
    if address < 0
      throw new Error("Invalid memory address: #{address}")
    @memory[address] = value

  getParameter: (mode, offset) ->
    param = @getMemory(@instructionPointer + offset)
    switch mode
      when 0 # Position mode
        @getMemory(param)
      when 1 # Immediate mode
        param
      when 2 # Relative mode
        @getMemory(@relativeBase + param)
      else
        throw new Error("Invalid parameter mode: #{mode}")

  setParameter: (mode, offset, value) ->
    param = @getMemory(@instructionPointer + offset)
    switch mode
      when 0 # Position mode
        @setMemory(param, value)
      when 2 # Relative mode
        @setMemory(@relativeBase + param, value)
      else
        throw new Error("Invalid parameter mode for write: #{mode}")

  addInput: (input) ->
    @inputBuffer.push(input)

  getOutput: () ->
    @outputBuffer.shift()

  hasOutput: () ->
    @outputBuffer.length > 0

  run: () ->
    while not @halted
      instruction = @getMemory(@instructionPointer)
      opcode = instruction % 100
      mode1 = Math.floor(instruction / 100) % 10
      mode2 = Math.floor(instruction / 1000) % 10
      mode3 = Math.floor(instruction / 10000) % 10

      switch opcode
        when 1 # Add
          val1 = @getParameter(mode1, 1)
          val2 = @getParameter(mode2, 2)
          @setParameter(mode3, 3, val1 + val2)
          @instructionPointer += 4
        when 2 # Multiply
          val1 = @getParameter(mode1, 1)
          val2 = @getParameter(mode2, 2)
          @setParameter(mode3, 3, val1 * val2)
          @instructionPointer += 4
        when 3 # Input
          if @inputBuffer.length == 0
            return # Wait for input
          input = @inputBuffer.shift()
          @setParameter(mode1, 1, input)
          @instructionPointer += 2
        when 4 # Output
          output = @getParameter(mode1, 1)
          @outputBuffer.push(output)
          @instructionPointer += 2
        when 5 # Jump-if-true
          val1 = @getParameter(mode1, 1)
          val2 = @getParameter(mode2, 2)
          if val1 != 0
            @instructionPointer = val2
          else
            @instructionPointer += 3
        when 6 # Jump-if-false
          val1 = @getParameter(mode1, 1)
          val2 = @getParameter(mode2, 2)
          if val1 == 0
            @instructionPointer = val2
          else
            @instructionPointer += 3
        when 7 # Less than
          val1 = @getParameter(mode1, 1)
          val2 = @getParameter(mode2, 2)
          @setParameter(mode3, 3, if val1 < val2 then 1 else 0)
          @instructionPointer += 4
        when 8 # Equals
          val1 = @getParameter(mode1, 1)
          val2 = @getParameter(mode2, 2)
          @setParameter(mode3, 3, if val1 == val2 then 1 else 0)
          @instructionPointer += 4
        when 9 # Adjust relative base
          val1 = @getParameter(mode1, 1)
          @relativeBase += val1
          @instructionPointer += 2
        when 99 # Halt
          @halted = true
        else
          throw new Error("Invalid opcode: #{opcode}")

# Read program from file
programString = fs.readFileSync('input.txt', 'utf8').trim()
program = programString.split(',').map(Number)

# Part 1
computer1 = new IntcodeComputer(program)
computer1.addInput(1)
computer1.run()
console.log("Part 1 Output:", computer1.getOutput())

# Part 2
computer2 = new IntcodeComputer(program)
computer2.addInput(2)
computer2.run()
console.log("Part 2 Output:", computer2.getOutput())

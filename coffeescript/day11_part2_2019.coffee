
# CoffeeScript solution for Advent of Code Day 11: Space Police

fs = require 'fs'

# --- Intcode Computer ---
# Based on previous days' Intcode implementations, supporting relative mode.
class IntcodeComputer
  constructor: (programCode) ->
    @memory = {}
    for code, i in programCode
      @memory[i] = code
    @ip = 0
    @relativeBase = 0
    @halted = false
    @inputQueue = []
    @outputQueue = []

  # Helper to get value based on parameter mode
  getParameter: (offset, mode) ->
    param = @memory[@ip + offset] ? 0
    switch mode
      when 0 # Position mode
        @memory[param] ? 0
      when 1 # Immediate mode
        param
      when 2 # Relative mode
        @memory[@relativeBase + param] ? 0
      else
        throw new Error "Invalid parameter mode: #{mode}"

  # Helper to get address based on parameter mode (for writing)
  getAddress: (offset, mode) ->
    param = @memory[@ip + offset] ? 0
    switch mode
      when 0 # Position mode
        param
      when 2 # Relative mode
        @relativeBase + param
      else
        # Immediate mode is never valid for write addresses
        throw new Error "Invalid address mode for writing: #{mode}"

  # Add input value(s) to the queue
  addInput: (value...) ->
    @inputQueue.push value...

  # Run the Intcode program until it halts or needs input
  run: ->
    loop
      instruction = @memory[@ip] ? 0
      opcode = instruction % 100
      mode1 = Math.floor(instruction / 100) % 10
      mode2 = Math.floor(instruction / 1000) % 10
      mode3 = Math.floor(instruction / 10000) % 10

      switch opcode
        when 1 # Add
          val1 = @getParameter(1, mode1)
          val2 = @getParameter(2, mode2)
          addr = @getAddress(3, mode3)
          @memory[addr] = val1 + val2
          @ip += 4
        when 2 # Multiply
          val1 = @getParameter(1, mode1)
          val2 = @getParameter(2, mode2)
          addr = @getAddress(3, mode3)
          @memory[addr] = val1 * val2
          @ip += 4
        when 3 # Input
          if @inputQueue.length == 0
            # Need input, pause execution
            return 'needs_input'
          inputValue = @inputQueue.shift()
          addr = @getAddress(1, mode1)
          @memory[addr] = inputValue
          @ip += 2
        when 4 # Output
          outputValue = @getParameter(1, mode1)
          @outputQueue.push outputValue
          @ip += 2
          # Return after producing output, allowing external processing
          return 'output_ready'
        when 5 # Jump-if-true
          val1 = @getParameter(1, mode1)
          val2 = @getParameter(2, mode2)
          if val1 != 0
            @ip = val2
          else
            @ip += 3
        when 6 # Jump-if-false
          val1 = @getParameter(1, mode1)
          val2 = @getParameter(2, mode2)
          if val1 == 0
            @ip = val2
          else
            @ip += 3
        when 7 # Less than
          val1 = @getParameter(1, mode1)
          val2 = @getParameter(2, mode2)
          addr = @getAddress(3, mode3)
          @memory[addr] = if val1 < val2 then 1 else 0
          @ip += 4
        when 8 # Equals
          val1 = @getParameter(1, mode1)
          val2 = @getParameter(2, mode2)
          addr = @getAddress(3, mode3)
          @memory[addr] = if val1 == val2 then 1 else 0
          @ip += 4
        when 9 # Adjust relative base
          val1 = @getParameter(1, mode1)
          @relativeBase += val1
          @ip += 2
        when 99 # Halt
          @halted = true
          return 'halted'
        else
          throw new Error "Unknown opcode: #{opcode} at IP #{@ip}"

# --- Robot Simulation ---
runRobot = (programCode, startColor) ->
  computer = new IntcodeComputer programCode.slice() # Use a copy for each run
  panels = {} # Dictionary to store panel colors: key="x,y", value=0(black) or 1(white)
  x = 0
  y = 0
  # Directions: 0: up, 1: right, 2: down, 3: left
  direction = 0
  paintedCount = 0

  # Set starting panel color if specified
  panels["#{x},#{y}"] = startColor

  loop
    # 1. Get current panel color (default to black 0)
    currentPanelKey = "#{x},#{y}"
    currentColor = panels[currentPanelKey] ? 0

    # 2. Provide color to Intcode computer
    computer.addInput currentColor
    
    # 3. Run computer until it produces two outputs or halts/needs input
    outputs = []
    while outputs.length < 2 and not computer.halted
        status = computer.run()
        if status == 'output_ready'
            outputs.push computer.outputQueue.pop() # Get the latest output
        else if status == 'needs_input' and outputs.length < 2
            # Should not happen if logic is correct, robot always provides input
            throw new Error "Computer unexpectedly needs input mid-cycle"
        else if status == 'halted'
             break # Exit the inner loop if halted

    # Break outer loop if computer halted
    break if computer.halted

    # Check if we got the expected two outputs
    if outputs.length < 2
        console.error "Computer halted prematurely or didn't produce two outputs. Outputs:", outputs
        break
        
    # 4. Process outputs
    paintColor = outputs[0]
    turnDirection = outputs[1]

    # 5. Paint the panel
    # Check if this panel was painted before (regardless of color change)
    paintedCount++ if panels[currentPanelKey] == undefined 
    panels[currentPanelKey] = paintColor

    # 6. Turn the robot
    if turnDirection == 0 # Turn left 90 degrees
      direction = (direction - 1 + 4) % 4
    else # Turn right 90 degrees (turnDirection == 1)
      direction = (direction + 1) % 4

    # 7. Move forward one panel
    switch direction
      when 0 # Up
        y -= 1
      when 1 # Right
        x += 1
      when 2 # Down
        y += 1
      when 3 # Left
        x -= 1
      
  return {panels, paintedCount}

# --- Main Execution ---
main = ->
  try
    # Read input file
    input = fs.readFileSync('input.txt', 'utf8').trim()
    programCode = input.split(',').map Number

    # --- Part 1 ---
    resultPart1 = runRobot(programCode, 0) # Start on black panel
    # The number of panels painted at least once is the size of the panels dictionary
    console.log "Part 1: Number of panels painted at least once:", Object.keys(resultPart1.panels).length

    # --- Part 2 ---
    resultPart2 = runRobot(programCode, 1) # Start on white panel
    panels = resultPart2.panels

    # Determine grid boundaries
    minX = Infinity
    maxX = -Infinity
    minY = Infinity
    maxY = -Infinity
    for key of panels
      [x, y] = key.split(',').map Number
      minX = Math.min minX, x
      maxX = Math.max maxX, x
      minY = Math.min minY, y
      maxY = Math.max maxY, y

    # Build the output grid string
    outputGrid = []
    for y in [minY..maxY]
      row = ""
      for x in [minX..maxX]
        key = "#{x},#{y}"
        color = panels[key] ? 0 # Default to black if not painted
        row += if color == 1 then "#" else " " # Use # for white, space for black
      outputGrid.push row

    console.log "\nPart 2: Registration Identifier:"
    console.log outputGrid.join("\n")

  catch error
    console.error "Error:", error
    process.exit(1) # Indicate failure

# Run the main function
main()

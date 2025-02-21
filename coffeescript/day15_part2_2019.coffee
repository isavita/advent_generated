
fs = require 'fs'

class IntcodeComputer
  constructor: (@memory, @inputs = [], @outputs = [], @relativeBase = 0, @halted = false) ->
    @memory = @memory.slice() # Create a copy to avoid modifying the original
    @ip = 0 # Instruction pointer

  run: ->
    while @ip < @memory.length and not @halted
      opcode = @memory[@ip] % 100
      modes = Math.floor(@memory[@ip] / 100).toString().split('').reverse().map(Number)

      param = (offset) =>
        mode = modes[offset - 1] || 0
        switch mode
          when 0 # Position mode
            @memory[@memory[@ip + offset] || 0] || 0
          when 1 # Immediate mode
            @memory[@ip + offset] || 0
          when 2 # Relative mode
            @memory[(@memory[@ip + offset] || 0) + @relativeBase] || 0

      writeParam = (offset, value) =>
        mode = modes[offset - 1] || 0
        address = switch mode
          when 0 # Position mode
            @memory[@ip + offset] || 0
          when 2 # Relative mode
            (@memory[@ip + offset] || 0) + @relativeBase
        @memory[address] = value

      switch opcode
        when 1 # Add
          writeParam(3, param(1) + param(2))
          @ip += 4
        when 2 # Multiply
          writeParam(3, param(1) * param(2))
          @ip += 4
        when 3 # Input
          if @inputs.length > 0
            writeParam(1, @inputs.shift())
            @ip += 2
          else
            return  # Pause execution if no input is available
        when 4 # Output
          @outputs.push(param(1))
          @ip += 2
          return # Pause after outputting
        when 5 # Jump-if-true
          @ip = if param(1) != 0 then param(2) else @ip + 3
        when 6 # Jump-if-false
          @ip = if param(1) == 0 then param(2) else @ip + 3
        when 7 # Less than
          writeParam(3, if param(1) < param(2) then 1 else 0)
          @ip += 4
        when 8 # Equals
          writeParam(3, if param(1) == param(2) then 1 else 0)
          @ip += 4
        when 9 # Adjust relative base
          @relativeBase += param(1)
          @ip += 2
        when 99 # Halt
          @halted = true
        else
          throw new Error("Unknown opcode: #{opcode}")
    return

  addInput: (input) ->
    @inputs.push(input)

  getOutput: ->
    @outputs.shift()

  isHalted: ->
    @halted


# --- Main program ---

solve = (program) ->
  # Part 1: Find shortest path to oxygen system
  grid = {} # key: "x,y", value: 0 (wall), 1 (empty), 2 (oxygen)
  droidPos = {x: 0, y: 0}
  grid["#{droidPos.x},#{droidPos.y}"] = 1
  queue = [{x: droidPos.x, y: droidPos.y, path: [], computer: new IntcodeComputer(program)}]
  oxygenPos = null
  visited = new Set()
  visited.add("0,0")

  while queue.length > 0
    currentState = queue.shift()
    {x, y, path, computer} = currentState

    for direction in [1, 2, 3, 4] # North, South, West, East
      dx = 0
      dy = 0
      switch direction
        when 1 then dy = -1  # North
        when 2 then dy = 1   # South
        when 3 then dx = -1  # West
        when 4 then dx = 1   # East

      newX = x + dx
      newY = y + dy
      newPosStr = "#{newX},#{newY}"

      continue if visited.has(newPosStr)
      visited.add(newPosStr)

      newComputer = new IntcodeComputer(computer.memory, [], [], computer.relativeBase, computer.halted)
      newComputer.ip = computer.ip

      newComputer.addInput(direction)
      newComputer.run()
      status = newComputer.getOutput()

      grid[newPosStr] = status
      newPath = path.concat([direction])

      if status != 0
        if status == 2
          oxygenPos = {x: newX, y: newY}
          console.log "Part 1: Fewest movements:", newPath.length
          break
        else
          queue.push({x: newX, y: newY, path: newPath, computer: newComputer})
    continue if oxygenPos?

  # Part 2: Calculate time to fill with oxygen

  minutes = 0
  oxygenated = new Set()
  oxygenated.add("#{oxygenPos.x},#{oxygenPos.y}")
  queue = [oxygenPos]

  while queue.length > 0
    nextQueue = []
    for pos in queue
        for direction in [1,2,3,4]
            dx = 0
            dy = 0
            switch direction
              when 1 then dy = -1
              when 2 then dy = 1
              when 3 then dx = -1
              when 4 then dx = 1

            newX = pos.x + dx
            newY = pos.y + dy
            newPosStr = "#{newX},#{newY}"
            
            if grid[newPosStr] == 1 and not oxygenated.has(newPosStr)
                oxygenated.add(newPosStr)
                nextQueue.push({x:newX, y:newY})

    if nextQueue.length > 0
        minutes++
        queue = nextQueue
    else
        break


  console.log "Part 2: Minutes to fill:", minutes




input = fs.readFileSync('input.txt', 'utf8').trim()
program = input.split(',').map(Number)
solve(program)

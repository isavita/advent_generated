
class IntcodeComputer
  constructor: (@memory, @inputQueue = [], @outputQueue = [], @relativeBase = 0) ->
    @ip = 0
    @halted = false
    @memory = @memory.slice() # Create a copy

  run: ->
    while !@halted
      opcode = @memory[@ip] % 100
      modes = Math.floor(@memory[@ip] / 100)

      param = (n) =>
        mode = Math.floor(modes / (10 ** (n - 1))) % 10
        switch mode
          when 0 then @memory[@memory[@ip + n] || 0] || 0  # Position mode
          when 1 then @memory[@ip + n] || 0                # Immediate mode
          when 2 then @memory[(@memory[@ip + n] || 0) + @relativeBase] || 0 # Relative mode
          else throw new Error("Invalid parameter mode: #{mode}")

      setParam = (n, value) =>
        mode = Math.floor(modes / (10 ** (n - 1))) % 10
        switch mode
          when 0 then @memory[@memory[@ip + n] || 0] = value
          when 1 then throw new Error("Immediate mode not allowed for setParam")
          when 2 then @memory[(@memory[@ip + n] || 0) + @relativeBase] = value
          else throw new Error("Invalid parameter mode: #{mode}")

      switch opcode
        when 1  # Add
          setParam(3, param(1) + param(2))
          @ip += 4
        when 2  # Multiply
          setParam(3, param(1) * param(2))
          @ip += 4
        when 3  # Input
          if @inputQueue.length > 0
            setParam(1, @inputQueue.shift())
            @ip += 2
          else
            return  # Wait for input
        when 4  # Output
          @outputQueue.push(param(1))
          @ip += 2
        when 5  # Jump-if-true
          @ip = if param(1) != 0 then param(2) else @ip + 3
        when 6  # Jump-if-false
          @ip = if param(1) == 0 then param(2) else @ip + 3
        when 7  # Less than
          setParam(3, if param(1) < param(2) then 1 else 0)
          @ip += 4
        when 8  # Equals
          setParam(3, if param(1) == param(2) then 1 else 0)
          @ip += 4
        when 9  # Adjust relative base
          @relativeBase += param(1)
          @ip += 2
        when 99 # Halt
          @halted = true
        else
          throw new Error("Invalid opcode: #{opcode}")

fs = require 'fs'

program = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number)

# BFS to explore the map and find the oxygen system
explore = (program) ->
    queue = [{ x: 0, y: 0, steps: 0, computer: new IntcodeComputer(program) }]
    visited = { '0,0': true }
    oxygenSystemLocation = null
    
    directions = [null, [0, -1], [0, 1], [-1, 0], [1, 0]] # Null, North, South, West, East

    while queue.length > 0
        currentState = queue.shift()

        for direction in [1..4]  # Try all directions
            dx = directions[direction][0]
            dy = directions[direction][1]
            newX = currentState.x + dx
            newY = currentState.y + dy
            newSteps = currentState.steps + 1

            if visited["#{newX},#{newY}"]
                continue
            visited["#{newX},#{newY}"] = true
            
            newComputer = new IntcodeComputer(currentState.computer.memory, [], [], currentState.computer.relativeBase) #copy state
            newComputer.ip = currentState.computer.ip #copy instruction pointer too
            
            newComputer.inputQueue.push(direction)
            newComputer.run()
            status = newComputer.outputQueue.shift()

            switch status
                when 0  # Wall
                    continue
                when 1  # Moved
                    queue.push({ x: newX, y: newY, steps: newSteps, computer: newComputer })
                when 2  # Oxygen system
                    oxygenSystemLocation = {x: newX, y: newY, steps: newSteps}
                    return oxygenSystemLocation  # Return immediately upon finding it

result = explore(program)
console.log result.steps

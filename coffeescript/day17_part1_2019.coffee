
fs = require 'fs'

class IntcodeComputer
  constructor: (@memory, @input = [], @output = [], @relativeBase = 0) ->
    @memory = @memory.slice() # Create a copy to avoid modifying the original
    @ip = 0

  run: ->
    while true
      opcode = @memory[@ip] % 100
      modes = Math.floor(@memory[@ip] / 100).toString().split('').reverse().map(Number)

      param = (n) =>
        mode = modes[n - 1] || 0
        switch mode
          when 0 then @memory[@memory[@ip + n] || 0] || 0
          when 1 then @memory[@ip + n] || 0
          when 2 then @memory[(@memory[@ip + n] || 0) + @relativeBase] || 0

      setParam = (n, value) =>
          mode = modes[n-1] || 0
          address = @memory[@ip + n] || 0
          if mode == 2 then address += @relativeBase
          @memory[address] = value
          

      switch opcode
        when 1 # Add
          setParam(3, param(1) + param(2))
          @ip += 4
        when 2 # Multiply
          setParam(3, param(1) * param(2))
          @ip += 4
        when 3 # Input
          if @input.length > 0
            setParam(1,@input.shift())
            @ip += 2
          else
            return null # Halt if no input
            
        when 4 # Output
          @output.push param(1)
          @ip += 2
        when 5 # Jump-if-true
          @ip = if param(1) != 0 then param(2) else @ip + 3
        when 6 # Jump-if-false
          @ip = if param(1) == 0 then param(2) else @ip + 3
        when 7 # Less than
            setParam(3, if param(1) < param(2) then 1 else 0)
            @ip += 4
        when 8 # Equals
            setParam(3, if param(1) == param(2) then 1 else 0)
            @ip += 4
        when 9 # Adjust relative base
          @relativeBase += param(1)
          @ip += 2
        when 99 # Halt
          return @output
        else
          throw new Error "Invalid opcode: #{opcode}"

readInput = (filename) ->
  fs.readFileSync(filename, 'utf8').trim().split(',').map(Number)

solvePart1 = (program) ->
  computer = new IntcodeComputer(program)
  output = computer.run()

  grid = []
  row = []
  for charCode in output
    if charCode == 10
      grid.push row
      row = []
    else
      row.push String.fromCharCode(charCode)
  grid.pop() if grid[grid.length-1].length == 0 #remove empty last row if exists

  alignmentSum = 0
  for y in [1...grid.length - 1]
    for x in [1...grid[y].length - 1]
      if grid[y][x] == '#' and grid[y - 1][x] == '#' and grid[y + 1][x] == '#' and grid[y][x - 1] == '#' and grid[y][x + 1] == '#'
        alignmentSum += x * y

  alignmentSum

# Main execution
program = readInput('input.txt')
result1 = solvePart1(program)
console.log "Part 1: #{result1}"

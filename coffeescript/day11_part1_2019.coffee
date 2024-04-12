fs = require 'fs'
path = require 'path'

Black = 0
White = 1
[Up, Right, Down, Left] = [0, 1, 2, 3]

class Position
  constructor: (@x, @y) ->

  hash: -> "#{@x},#{@y}"

class Robot
  constructor: (@position = new Position(0, 0), @direction = Up) ->

  turnAndMove: (turnDirection) ->
    @direction = (@direction + (if turnDirection == 0 then 3 else 1)) % 4
    switch @direction
      when Up then @position.y--
      when Right then @position.x++
      when Down then @position.y++
      when Left then @position.x--

class Grid
  constructor: ->
    @panels = {}

  getColor: (position) ->
    @panels[position.hash()] or Black

  setColor: (position, color) ->
    @panels[position.hash()] = color

  count: ->
    Object.keys(@panels).length

class Intcode
  constructor: (program) ->
    @memory = program.slice()
    @ip = 0
    @input = []
    @output = []
    @halted = false

  addInput: (input) ->
    @input.push input

  run: ->
    while not @halted
      opcode = @memory[@ip] % 100
      switch opcode
        when 1, 2, 7, 8
          params = @getParams 3
          val1 = @readMemory params[0]
          val2 = @readMemory params[1]
          if opcode == 1
            @writeMemory params[2], val1 + val2
          else if opcode == 2
            @writeMemory params[2], val1 * val2
          else if (opcode == 7 and val1 < val2) or (opcode == 8 and val1 == val2)
            @writeMemory params[2], 1
          else
            @writeMemory params[2], 0
          @ip += 4
        when 3, 4
          params = @getParams 1
          if opcode == 3
            return if @input.length == 0
            @writeMemory params[0], @input.shift()
          else
            @output.push @readMemory params[0]
          @ip += 2
        when 5, 6
          params = @getParams 2
          val = @readMemory params[0]
          target = @readMemory params[1]
          if (opcode == 5 and val != 0) or (opcode == 6 and val == 0)
            @ip = target
          else
            @ip += 3
        when 99
          @halted = true
          return
        else
          throw "unknown opcode: #{opcode}"

  readMemory: (address) ->
    @ensureMemory address
    @memory[address]

  writeMemory: (address, value) ->
    @ensureMemory address
    @memory[address] = value

  ensureMemory: (address) ->
    if address >= @memory.length
      newMemory = Array(address + 1).fill(0)
      newMemory[...@memory.length] = @memory
      @memory = newMemory

  getParams: (count) ->
    paramModes = Math.floor(@memory[@ip] / 100)
    params = []
    for i in [0...count]
      if paramModes % 10 == 1
        params.push @ip + i + 1
      else
        params.push @memory[@ip + i + 1]
      paramModes = Math.floor(paramModes / 10)
    params

main = ->
  data = fs.readFileSync path.join(__dirname, 'input.txt'), 'utf8'
  program = data.trim().split(',').map (s) -> parseInt(s, 10)

  grid = new Grid()
  robot = new Robot()
  intcode = new Intcode(program)

  while not intcode.halted
    currentColor = grid.getColor(robot.position)
    intcode.addInput currentColor
    intcode.run()
    outputs = intcode.output.splice 0, intcode.output.length

    if outputs.length == 2
      grid.setColor robot.position, outputs[0]
      robot.turnAndMove outputs[1]

  count = grid.count()
  console.log count

main()
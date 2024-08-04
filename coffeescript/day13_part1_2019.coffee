fs = require 'fs'

class Machine
  constructor: (@data, @in, @out) ->
    @ip = 0
    @relbase = 0

  get: (i, mo) ->
    switch mo
      when 1 then @data[i]
      when 0 then @data[@data[i]]
      when 2 then @data[@relbase + @data[i]]
      else throw new Error "Unknown mode: #{mo}"

  set: (i, mo, val) ->
    switch mo
      when 0 then @data[@data[i]] = val
      when 2 then @data[@relbase + @data[i]] = val
      else throw new Error "Unknown mode: #{mo}"

  decode: (n) ->
    op = n % 100
    n = Math.floor(n / 100)
    modes = [0, 0, 0]
    for i in [0..2]
      modes[i] = n % 10
      n = Math.floor(n / 10)
    [op, modes]

  step: ->
    [op, modes] = @decode(@data[@ip])
    switch op
      when 1
        val = @get(@ip+1, modes[0]) + @get(@ip+2, modes[1])
        @set(@ip+3, modes[2], val)
        @ip += 4
      when 2
        val = @get(@ip+1, modes[0]) * @get(@ip+2, modes[1])
        @set(@ip+3, modes[2], val)
        @ip += 4
      when 3
        @set(@ip+1, modes[0], @in.shift())
        @ip += 2
      when 4
        @out.push(@get(@ip+1, modes[0]))
        @ip += 2
      when 5
        if @get(@ip+1, modes[0]) != 0
          @ip = @get(@ip+2, modes[1])
        else
          @ip += 3
      when 6
        if @get(@ip+1, modes[0]) == 0
          @ip = @get(@ip+2, modes[1])
        else
          @ip += 3
      when 7
        if @get(@ip+1, modes[0]) < @get(@ip+2, modes[1])
          @set(@ip+3, modes[2], 1)
        else
          @set(@ip+3, modes[2], 0)
        @ip += 4
      when 8
        if @get(@ip+1, modes[0]) == @get(@ip+2, modes[1])
          @set(@ip+3, modes[2], 1)
        else
          @set(@ip+3, modes[2], 0)
        @ip += 4
      when 9
        @relbase += @get(@ip+1, modes[0])
        @ip += 2
      when 99
        return false
      else
        throw new Error "Unknown opcode: #{op}"
    true

  run: ->
    while @step()
      continue
    return

countBlocks = (program) ->
  grid = {}
  in_ = []
  out = []
  machine = new Machine(program, in_, out)
  machine.run()
  for i in [0...out.length] by 3
    x = out[i]
    y = out[i+1]
    grid["#{x},#{y}"] = out[i+2]
  n = 0
  for key, value of grid
    if value == 2
      n++
  n

main = ->
  input = fs.readFileSync('input.txt', 'utf8').trim()
  program = input.split(',').map(Number)
  console.log(countBlocks(program))

main()
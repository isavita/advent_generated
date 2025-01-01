
fs = require 'fs'

class VM
  constructor: (@code, @ip = 0, @relativeBase = 0) ->
    @input = []
    @output = []

  load: (filename) ->
    data = fs.readFileSync(filename, 'utf-8').trim()
    @code = data.split(',').map(Number)

  run: ->
    while true
      cmd = @code[@ip]
      opcode = cmd % 100
      modes = Math.floor(cmd / 100)
      arity = 0
      params = []

      switch opcode
        when 1 # add
          arity = 3
          params = @getParamsAddresses(modes, arity)
          @code[params[2]] = @code[params[0]] + @code[params[1]]
        when 2 # multiply
          arity = 3
          params = @getParamsAddresses(modes, arity)
          @code[params[2]] = @code[params[0]] * @code[params[1]]
        when 3 # read
          arity = 1
          params = @getParamsAddresses(modes, arity)
          @code[params[0]] = @input.shift()
        when 4 # write
          arity = 1
          params = @getParamsAddresses(modes, arity)
          @output.push @code[params[0]]
        when 5 # jump not zero
          arity = 2
          params = @getParamsAddresses(modes, arity)
          if @code[params[0]] != 0
            @ip = @code[params[1]]
            continue
        when 6 # jump zero
          arity = 2
          params = @getParamsAddresses(modes, arity)
          if @code[params[0]] == 0
            @ip = @code[params[1]]
            continue
        when 7 # less than
          arity = 3
          params = @getParamsAddresses(modes, arity)
          @code[params[2]] = if @code[params[0]] < @code[params[1]] then 1 else 0
        when 8 # equal
          arity = 3
          params = @getParamsAddresses(modes, arity)
          @code[params[2]] = if @code[params[0]] == @code[params[1]] then 1 else 0
        when 9 # change relative base
          arity = 1
          params = @getParamsAddresses(modes, arity)
          @relativeBase += @code[params[0]]
        when 99 # halt
          return
        else
          throw new Error "Invalid opcode: #{opcode}"

      @ip += arity + 1

  getParamsAddresses: (modes, arity) ->
    params = []
    for i in [0...arity]
      mode = Math.floor(modes / Math.pow(10, i)) % 10
      params.push @getParamAddress(@ip + i + 1, mode)
    params

  getParamAddress: (pos, mode) ->
    switch mode
      when 0 then @code[pos] or 0
      when 1 then pos
      when 2 then (@relativeBase + (@code[pos] or 0))
      else throw new Error "Invalid mode: #{mode}"

sendString = (vm, s) ->
  for char in s
    vm.input.push char.charCodeAt(0)
  vm.input.push 10

vm = new VM()
vm.load('input.txt')

instructions = [
  "NOT A J",
  "NOT B T",
  "OR T J",
  "NOT C T",
  "OR T J",
  "AND D J",
  "WALK",
]

for instruction in instructions
  sendString(vm, instruction)

vm.run()

for output in vm.output
  if output > 127
    console.log output
    break

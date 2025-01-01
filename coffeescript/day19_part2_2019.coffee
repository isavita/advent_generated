
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

      switch opcode
        when 1 # add
          arity = 3
          params = @getParamsAddresses(@ip, modes, arity)
          @code[params[2]] = @code[params[0]] + @code[params[1]]
        when 2 # multiply
          arity = 3
          params = @getParamsAddresses(@ip, modes, arity)
          @code[params[2]] = @code[params[0]] * @code[params[1]]
        when 3 # read
          arity = 1
          params = @getParamsAddresses(@ip, modes, arity)
          @code[params[0]] = @input.shift()
        when 4 # write
          arity = 1
          params = @getParamsAddresses(@ip, modes, arity)
          @output.push @code[params[0]]
        when 5 # jump not zero
          arity = 2
          params = @getParamsAddresses(@ip, modes, arity)
          if @code[params[0]] != 0
            @ip = @code[params[1]]
            continue
        when 6 # jump zero
          arity = 2
          params = @getParamsAddresses(@ip, modes, arity)
          if @code[params[0]] == 0
            @ip = @code[params[1]]
            continue
        when 7 # less than
          arity = 3
          params = @getParamsAddresses(@ip, modes, arity)
          @code[params[2]] = if @code[params[0]] < @code[params[1]] then 1 else 0
        when 8 # equal
          arity = 3
          params = @getParamsAddresses(@ip, modes, arity)
          @code[params[2]] = if @code[params[0]] == @code[params[1]] then 1 else 0
        when 9 # change relative base
          arity = 1
          params = @getParamsAddresses(@ip, modes, arity)
          @relativeBase += @code[params[0]]
        when 99 # halt
          return
        else
          throw new Error "Invalid opcode: #{opcode}"

      @ip += arity + 1

  getParamsAddresses: (pos, modes, arity) ->
    results = []
    for i in [0...arity]
      mode = Math.floor(modes / (10 ** i)) % 10
      results.push @getParamAddress(pos + i + 1, mode)
    results

  getParamAddress: (pos, mode) ->
    switch mode
      when 0 # Position
        @code[pos] or 0
      when 1 # Immediate
        pos
      when 2 # Relative
        @relativeBase + (@code[pos] or 0)
      else
        throw new Error "Invalid mode: #{mode}"

beam = (x, y, code) ->
  vm = new VM(code.slice())
  vm.input = [x, y]
  vm.run()
  vm.output[0] == 1

main = ->
  code = null
  try
    data = fs.readFileSync('input.txt', 'utf-8').trim()
    code = data.split(',').map(Number)
  catch error
    console.error "Error reading input file:", error
    return

  y = 20
  x = 0

  while true
    if not beam(x, y, code)
      x++
      continue

    if not beam(x + 99, y, code)
      y++
      continue

    if not beam(x, y + 99, code)
      x++
      continue

    console.log x * 10000 + y
    return

main()

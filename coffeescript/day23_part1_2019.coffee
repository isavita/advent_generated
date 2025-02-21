
fs = require 'fs'

class IntcodeComputer
  constructor: (@program, @inputs = []) ->
    @memory = {}
    for i, v of @program
      @memory[i] = v
    @ip = 0
    @relative_base = 0
    @outputs = []
    @halted = false
    @needs_input = false

  get_param: (mode, offset) ->
    switch mode
      when 0 then @memory[@memory[@ip + offset]] or 0
      when 1 then @memory[@ip + offset] or 0
      when 2 then @memory[@relative_base + (@memory[@ip + offset] or 0)] or 0

  set_param: (mode, offset, value) ->
    switch mode
      when 0 then @memory[@memory[@ip + offset]] = value
      when 2 then @memory[@relative_base + (@memory[@ip + offset] or 0)] = value

  run: ->
    loop
      opcode = @memory[@ip] % 100
      modes = [
        Math.floor(@memory[@ip] / 100) % 10
        Math.floor(@memory[@ip] / 1000) % 10
        Math.floor(@memory[@ip] / 10000) % 10
      ]

      if opcode is 99
        @halted = true
        break

      if opcode in [1, 2, 7, 8]
        param1 = @get_param(modes[0], 1)
        param2 = @get_param(modes[1], 2)
        result = switch opcode
          when 1 then param1 + param2
          when 2 then param1 * param2
          when 7 then if param1 < param2 then 1 else 0
          when 8 then if param1 is param2 then 1 else 0
        @set_param(modes[2], 3, result)
        @ip += 4
      else if opcode is 3
        if @inputs.length is 0
            @needs_input = true
            return
        @needs_input = false
        @set_param(modes[0], 1, @inputs.shift())
        @ip += 2
      else if opcode is 4
        @outputs.push @get_param(modes[0], 1)
        @ip += 2
        return if @outputs.length is 3
      else if opcode in [5, 6]
        param1 = @get_param(modes[0], 1)
        param2 = @get_param(modes[1], 2)
        if (opcode is 5 and param1 isnt 0) or (opcode is 6 and param1 is 0)
          @ip = param2
        else
          @ip += 3
      else if opcode is 9
        @relative_base += @get_param(modes[0], 1)
        @ip += 2
      else
        throw new Error("Unknown opcode #{opcode}")

program = (fs.readFileSync('input.txt', 'utf8').trim().split(',')).map (x) -> parseInt(x)

computers = []
for address in [0...50]
    computers.push new IntcodeComputer(program.slice(), [address])

packet_queues = (new Array(50)).fill(null).map -> []

first_packet_to_255 = null

loop
    idle = true
    for computer, i in computers
        if packet_queues[i].length
            [x, y] = packet_queues[i].shift()
            computer.inputs.push x, y
        else
            computer.inputs.push -1

        computer.run()

        while computer.outputs.length >= 3
            idle = false
            [dest, x, y] = computer.outputs.splice(0, 3)

            if dest is 255
                if not first_packet_to_255?
                    first_packet_to_255 = y
                    console.log first_packet_to_255
                    process.exit(0)

            else if 0 <= dest < 50
                packet_queues[dest].push [x, y]

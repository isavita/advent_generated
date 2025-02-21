
fs = require 'fs'

class IntcodeComputer
  constructor: (program, inputs = []) ->
    @memory = {}
    for i, val of program
      @memory[i] = val
    @ip = 0
    @relative_base = 0
    @inputs = inputs
    @outputs = []
    @halted = false
    @idle = false

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
    while true
      opcode = @memory[@ip] % 100
      modes = [
        Math.floor(@memory[@ip] / 100) % 10
        Math.floor(@memory[@ip] / 1000) % 10
        Math.floor(@memory[@ip] / 10000) % 10
      ]

      if opcode is 99
        @halted = true
        break
      else if opcode in [1, 2, 7, 8]
        param1 = @get_param(modes[0], 1)
        param2 = @get_param(modes[1], 2)
        if opcode is 1
          result = param1 + param2
        else if opcode is 2
          result = param1 * param2
        else if opcode is 7
          result = if param1 < param2 then 1 else 0
        else if opcode is 8
          result = if param1 is param2 then 1 else 0
        @set_param(modes[2], 3, result)
        @ip += 4
      else if opcode is 3
        if @inputs.length is 0
          @set_param(modes[0], 1, -1)
          @ip += 2
          @idle = true
          return
        else
          value = @inputs.shift()
          @set_param(modes[0], 1, value)
          @ip += 2
          @idle = false
      else if opcode is 4
        param1 = @get_param(modes[0], 1)
        @outputs.push param1
        @ip += 2
        @idle = false
        if @outputs.length is 3
          return
      else if opcode in [5, 6]
        param1 = @get_param(modes[0], 1)
        param2 = @get_param(modes[1], 2)
        if (opcode is 5 and param1 isnt 0) or (opcode is 6 and param1 is 0)
          @ip = param2
        else
          @ip += 3
      else if opcode is 9
        param1 = @get_param(modes[0], 1)
        @relative_base += param1
        @ip += 2
      else
        throw new Error("Unknown opcode #{opcode}")

program = (fs.readFileSync 'input.txt', 'utf8').trim().split(',').map (x) -> parseInt(x)

computers = []
for address in [0...50]
  computer = new IntcodeComputer program.slice(), [address]
  computers.push computer

packet_queues = (new Array(50)).fill(null).map -> []

nat_packet = null
prev_nat_y = null

while true
  network_idle = true
  for i, computer of computers
    if packet_queues[i].length > 0
      [x, y] = packet_queues[i].shift()
      computer.inputs.push x, y
      computer.idle = false
    else
      if computer.inputs.length == 0
              computer.inputs.push -1

    computer.run()

    while computer.outputs.length >= 3
      network_idle = false
      [dest, x, y] = computer.outputs.splice(0, 3)

      dest = parseInt dest
      x = parseInt x
      y = parseInt y

      if dest is 255
        nat_packet = [x, y]
      else if 0 <= dest < 50
        packet_queues[dest].push [x, y]

  if (for i in [0...50] then not packet_queues[i].length and computers[i].idle).every (x) -> x
    if nat_packet
      [x, y] = nat_packet
      packet_queues[0].push [x, y]
      if y is prev_nat_y
        console.log y
        process.exit 0
      prev_nat_y = y
    network_idle = false

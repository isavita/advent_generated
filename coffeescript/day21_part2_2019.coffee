fs = require 'fs'
input = fs.readFileSync('input.txt', 'utf8').trim().split(',').map (x) -> parseInt x, 10

class VM
  constructor: (@code, @ip = 0, @relativeBase = 0) ->
    @input = []
    @output = []

  run: ->
    while true
      cmd = @code[@ip]
      opcode = cmd % 100
      modes = (Math.floor(cmd / 100).toString().split('').reverse().map (x) -> parseInt x, 10)

      switch opcode
        when 1
          [a, b, c] = @getParams(3, modes)
          @code[c] = @code[a] + @code[b]
          @ip += 4
        when 2
          [a, b, c] = @getParams(3, modes)
          @code[c] = @code[a] * @code[b]
          @ip += 4
        when 3
          [a] = @getParams(1, modes)
          @code[a] = @input.shift()
          @ip += 2
        when 4
          [a] = @getParams(1, modes)
          @output.push(@code[a])
          @ip += 2
        when 5
          [a, b] = @getParams(2, modes)
          if @code[a] != 0
            @ip = @code[b]
          else
            @ip += 3
        when 6
          [a, b] = @getParams(2, modes)
          if @code[a] == 0
            @ip = @code[b]
          else
            @ip += 3
        when 7
          [a, b, c] = @getParams(3, modes)
          @code[c] = if @code[a] < @code[b] then 1 else 0
          @ip += 4
        when 8
          [a, b, c] = @getParams(3, modes)
          @code[c] = if @code[a] == @code[b] then 1 else 0
          @ip += 4
        when 9
          [a] = @getParams(1, modes)
          @relativeBase += @code[a]
          @ip += 2
        when 99
          return
        else
          throw new Error("Invalid opcode #{opcode}")

  getParams: (arity, modes) ->
    params = []
    for i in [0...arity]
      mode = if i < modes.length then modes[i] else 0
      switch mode
        when 0
          params.push(@code[@ip + i + 1])
        when 1
          params.push(@ip + i + 1)
        when 2
          params.push(@relativeBase + @code[@ip + i + 1])
    params

vm = new VM(input)

instructions = [
  "NOT A J",
  "NOT B T",
  "OR T J",
  "NOT C T",
  "OR T J",
  "AND D J",
  "NOT A T",
  "AND A T",
  "OR E T",
  "OR H T",
  "AND T J",
  "RUN"
]

for instruction in instructions
  for char in instruction
    vm.input.push(char.charCodeAt(0))
  vm.input.push(10)

vm.run()

console.log(vm.output)
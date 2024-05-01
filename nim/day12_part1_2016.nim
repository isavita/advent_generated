import strutils, sequtils, streams

var
  file = newFileStream("input.txt", fmRead)
  instructions: seq[string]
  line: string

while file.readLine(line):
  instructions.add(line)

var registers: array['a'..'d', int]
registers['a'] = 0
registers['b'] = 0
registers['c'] = 0
registers['d'] = 0

proc executeInstructions(instructions: seq[string], registers: var array['a'..'d', int]) =
  var i = 0
  while i < instructions.len:
    let parts = instructions[i].split_whitespace
    case parts[0]
    of "cpy":
      let val = if parts[1].all(isDigit): parseInt(parts[1]) else: registers[parts[1][0]]
      registers[parts[2][0]] = val
      inc i
    of "inc":
      inc registers[parts[1][0]]
      inc i
    of "dec":
      dec registers[parts[1][0]]
      inc i
    of "jnz":
      let val = if parts[1].all(isDigit): parseInt(parts[1]) else: registers[parts[1][0]]
      if val != 0:
        let jump = parseInt(parts[2])
        i += jump - 1
      inc i

executeInstructions(instructions, registers)

echo registers['a']
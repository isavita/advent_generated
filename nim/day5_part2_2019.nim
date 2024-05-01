import strutils, sequtils, strformat

proc getValue(program: var seq[int]; pos: int; mode: int): int =
  if mode == 0:
    return program[program[pos]]
  else:
    return program[pos]

proc main() =
  let file = readFile("input.txt")
  var program = file.split(',').map(parseInt)

  var i = 0
  var input = 5
  var output = 0

  while true:
    let opcode = program[i] mod 100
    let modes = program[i] div 100
    let param1Mode = modes mod 10
    let modes2 = modes div 10
    let param2Mode = modes2 mod 10

    case opcode
    of 1:
      let p1 = getValue(program, i + 1, param1Mode)
      let p2 = getValue(program, i + 2, param2Mode)
      let p3 = program[i + 3]
      program[p3] = p1 + p2
      i += 4
    of 2:
      let p1 = getValue(program, i + 1, param1Mode)
      let p2 = getValue(program, i + 2, param2Mode)
      let p3 = program[i + 3]
      program[p3] = p1 * p2
      i += 4
    of 3:
      program[program[i + 1]] = input
      i += 2
    of 4:
      output = getValue(program, i + 1, param1Mode)
      echo output
      i += 2
    of 5:
      let p1 = getValue(program, i + 1, param1Mode)
      let p2 = getValue(program, i + 2, param2Mode)
      if p1 != 0:
        i = p2
      else:
        i += 3
    of 6:
      let p1 = getValue(program, i + 1, param1Mode)
      let p2 = getValue(program, i + 2, param2Mode)
      if p1 == 0:
        i = p2
      else:
        i += 3
    of 7:
      let p1 = getValue(program, i + 1, param1Mode)
      let p2 = getValue(program, i + 2, param2Mode)
      let p3 = program[i + 3]
      if p1 < p2:
        program[p3] = 1
      else:
        program[p3] = 0
      i += 4
    of 8:
      let p1 = getValue(program, i + 1, param1Mode)
      let p2 = getValue(program, i + 2, param2Mode)
      let p3 = program[i + 3]
      if p1 == p2:
        program[p3] = 1
      else:
        program[p3] = 0
      i += 4
    of 99:
      return
    else:
      raise newException(IOError, "Invalid opcode")

when isMainModule:
  main()
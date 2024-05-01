import strutils, sequtils

type
  Opcode = enum
    Add = 1
    Multiply = 2
    Halt = 99

proc runProgram(program: var seq[int]): int =
  var ip = 0
  while ip < program.len:
    let opcode = Opcode(program[ip])
    case opcode
    of Add:
      let a = program[ip + 1]
      let b = program[ip + 2]
      let c = program[ip + 3]
      program[c] = program[a] + program[b]
      ip += 4
    of Multiply:
      let a = program[ip + 1]
      let b = program[ip + 2]
      let c = program[ip + 3]
      program[c] = program[a] * program[b]
      ip += 4
    of Halt:
      break
  program[0]

when isMainModule:
  let input = readFile("input.txt").strip().split(',')
  var program: seq[int] = @[]
  for num in input:
    program.add(parseInt(num))
  
  # Part 1
  program[1] = 12
  program[2] = 2
  echo "Part 1: ", runProgram(program)
  
  # Part 2
  for noun in 0..99:
    for verb in 0..99:
      var programCopy = readFile("input.txt").strip().split(',').map(parseInt)
      programCopy[1] = noun
      programCopy[2] = verb
      if runProgram(programCopy) == 19690720:
        echo "Part 2: ", 100 * noun + verb
        break
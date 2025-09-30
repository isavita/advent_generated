
import strutils, tables, deques

type
  Program = object
    registers: Table[char, int]
    pc: int
    queue: Deque[int]
    sendCount: int
    waiting: bool

proc getValue(registers: Table[char, int], s: string): int =
  if s[0] in 'a'..'z':
    registers.getOrDefault(s[0], 0)
  else:
    s.parseInt

proc runProgram(prog: var Program, instructions: seq[string], other: var Program): bool =
  if prog.pc < 0 or prog.pc >= instructions.len:
    return false

  let parts = instructions[prog.pc].splitWhitespace
  case parts[0]:
  of "snd":
    let val = prog.registers.getValue(parts[1])
    other.queue.addLast(val)
    prog.sendCount += 1
    prog.pc += 1
  of "set":
    let val = prog.registers.getValue(parts[2])
    prog.registers[parts[1][0]] = val
    prog.pc += 1
  of "add":
    let val = prog.registers.getValue(parts[2])
    prog.registers[parts[1][0]] = prog.registers.getOrDefault(parts[1][0], 0) + val
    prog.pc += 1
  of "mul":
    let val = prog.registers.getValue(parts[2])
    prog.registers[parts[1][0]] = prog.registers.getOrDefault(parts[1][0], 0) * val
    prog.pc += 1
  of "mod":
    let val = prog.registers.getValue(parts[2])
    prog.registers[parts[1][0]] = prog.registers.getOrDefault(parts[1][0], 0) mod val
    prog.pc += 1
  of "rcv":
    if prog.queue.len > 0:
      prog.registers[parts[1][0]] = prog.queue.popFirst()
      prog.waiting = false
      prog.pc += 1
    else:
      prog.waiting = true
      return false
  of "jgz":
    let checkVal = prog.registers.getValue(parts[1])
    if checkVal > 0:
      let jumpVal = prog.registers.getValue(parts[2])
      prog.pc += jumpVal
    else:
      prog.pc += 1
  else:
    discard
  
  return true

when isMainModule:
  let instructions = readFile("input.txt").strip.splitLines
  
  # Part 1
  var registers: Table[char, int]
  var pc = 0
  var lastSound = 0
  
  while pc >= 0 and pc < instructions.len:
    let parts = instructions[pc].splitWhitespace
    case parts[0]:
    of "snd":
      lastSound = registers.getValue(parts[1])
      pc += 1
    of "set":
      let val = registers.getValue(parts[2])
      registers[parts[1][0]] = val
      pc += 1
    of "add":
      let val = registers.getValue(parts[2])
      registers[parts[1][0]] = registers.getOrDefault(parts[1][0], 0) + val
      pc += 1
    of "mul":
      let val = registers.getValue(parts[2])
      registers[parts[1][0]] = registers.getOrDefault(parts[1][0], 0) * val
      pc += 1
    of "mod":
      let val = registers.getValue(parts[2])
      registers[parts[1][0]] = registers.getOrDefault(parts[1][0], 0) mod val
      pc += 1
    of "rcv":
      let val = registers.getValue(parts[1])
      if val != 0:
        echo lastSound
        break
      pc += 1
    of "jgz":
      let checkVal = registers.getValue(parts[1])
      if checkVal > 0:
        let jumpVal = registers.getValue(parts[2])
        pc += jumpVal
      else:
        pc += 1
    else:
      discard
  
  # Part 2
  var prog0 = Program(registers: {'p': 0}.toTable, pc: 0, queue: initDeque[int](), sendCount: 0, waiting: false)
  var prog1 = Program(registers: {'p': 1}.toTable, pc: 0, queue: initDeque[int](), sendCount: 0, waiting: false)
  
  while true:
    let p0Active = runProgram(prog0, instructions, prog1)
    let p1Active = runProgram(prog1, instructions, prog0)
    
    if not p0Active and not p1Active:
      break
    if prog0.waiting and prog1.waiting and prog0.queue.len == 0 and prog1.queue.len == 0:
      break
  
  echo prog1.sendCount

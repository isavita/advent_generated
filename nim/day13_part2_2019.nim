
import std/[tables, deques, strutils, parseutils]

type
  IntcodeComputer = object
    memory: Table[int64, int64]
    ip: int64
    relative_base: int64
    input_queue: Deque[int64]
    output_queue: Deque[int64]
    halted: bool
    waiting_for_input: bool

proc initIntcodeComputer(program: openArray[int64]): IntcodeComputer =
  for i, val in program:
    result.memory[i.int64] = val
  result.ip = 0
  result.relative_base = 0
  result.halted = false
  result.waiting_for_input = false

proc addInput(c: var IntcodeComputer, val: int64) =
  c.input_queue.addLast(val)
  c.waiting_for_input = false

proc run(c: var IntcodeComputer) =
  if c.halted: return

  template get_param(offset, mode: int64): int64 =
    let val_at_offset = c.memory.getOrDefault(c.ip + offset)
    case mode
    of 0: c.memory.getOrDefault(val_at_offset)
    of 1: val_at_offset
    of 2: c.memory.getOrDefault(c.relative_base + val_at_offset)
    else: 0'i64

  template get_write_addr(offset, mode: int64): int64 =
    let val_at_offset = c.memory.getOrDefault(c.ip + offset)
    case mode
    of 0: val_at_offset
    of 2: c.relative_base + val_at_offset
    else: 0'i64

  while true:
    let instruction = c.memory.getOrDefault(c.ip)
    let opcode = instruction mod 100
    let modes = [
      (instruction div 100) mod 10,
      (instruction div 1000) mod 10,
      (instruction div 10000) mod 10
    ]

    if opcode == 99:
      c.halted = true
      break

    case opcode
    of 1:
      let val1 = get_param(1, modes[0])
      let val2 = get_param(2, modes[1])
      let addr3 = get_write_addr(3, modes[2])
      c.memory[addr3] = val1 + val2
      c.ip += 4
    of 2:
      let val1 = get_param(1, modes[0])
      let val2 = get_param(2, modes[1])
      let addr3 = get_write_addr(3, modes[2])
      c.memory[addr3] = val1 * val2
      c.ip += 4
    of 3:
      if c.input_queue.len == 0:
        c.waiting_for_input = true
        return
      let addr1 = get_write_addr(1, modes[0])
      c.memory[addr1] = c.input_queue.popFirst()
      c.ip += 2
    of 4:
      let val1 = get_param(1, modes[0])
      c.output_queue.addLast(val1)
      c.ip += 2
      return
    of 5:
      let val1 = get_param(1, modes[0])
      let val2 = get_param(2, modes[1])
      c.ip = if val1 != 0: val2 else: c.ip + 3
    of 6:
      let val1 = get_param(1, modes[0])
      let val2 = get_param(2, modes[1])
      c.ip = if val1 == 0: val2 else: c.ip + 3
    of 7:
      let val1 = get_param(1, modes[0])
      let val2 = get_param(2, modes[1])
      let addr3 = get_write_addr(3, modes[2])
      c.memory[addr3] = if val1 < val2: 1 else: 0
      c.ip += 4
    of 8:
      let val1 = get_param(1, modes[0])
      let val2 = get_param(2, modes[1])
      let addr3 = get_write_addr(3, modes[2])
      c.memory[addr3] = if val1 == val2: 1 else: 0
      c.ip += 4
    of 9:
      let val1 = get_param(1, modes[0])
      c.relative_base += val1
      c.ip += 2
    else:
      c.halted = true
      break

proc hasOutput(c: IntcodeComputer): bool =
  c.output_queue.len > 0

proc getOutput(c: var IntcodeComputer): int64 =
  result = c.output_queue.popFirst()

proc isHalted(c: IntcodeComputer): bool =
  c.halted

proc isWaitingForInput(c: IntcodeComputer): bool =
  c.waiting_for_input

proc parseInput(filePath: string): seq[int64] =
  let content = readFile(filePath).strip()
  for part in content.split(','):
    result.add parseBiggestInt(part)

proc playGame(program_initial: seq[int64]): int64 =
  var computer = initIntcodeComputer(program_initial)
  computer.memory[0] = 2

  var score = 0'i64
  var ball_x = 0'i64
  var paddle_x = 0'i64

  while not computer.isHalted():
    computer.run()

    if computer.isHalted(): break

    if computer.isWaitingForInput():
      var input_val = 0'i64
      if ball_x > paddle_x: input_val = 1
      elif ball_x < paddle_x: input_val = -1
      computer.addInput(input_val)
      continue

    if computer.hasOutput():
      let x = computer.getOutput()
      computer.run()
      let y = computer.getOutput()
      computer.run()
      let tile_id = computer.getOutput()

      if x == -1 and y == 0:
        score = tile_id
      else:
        if tile_id == 3:
          paddle_x = x
        elif tile_id == 4:
          ball_x = x
  score

when isMainModule:
  let program = parseInput("input.txt")
  echo playGame(program)

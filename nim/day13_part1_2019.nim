
import std/[strutils, sequtils, tables, math]

# --- Intcode Computer ---

type
  IntcodeState = object
    mem: Table[int, int]
    ip: int
    relativeBase: int
    halted: bool
    output: seq[int]
    awaitingInput: bool

proc initIntcodeState(program: seq[int]): IntcodeState =
  result.mem = initTable[int, int]()
  for i, instruction in program:
    result.mem[i] = instruction
  result.ip = 0
  result.relativeBase = 0
  result.halted = false
  result.awaitingInput = false
  # result.output is implicitly initialized as empty seq

proc getOrDefault(mem: TableRef[int, int], address: int): int =
  # Intcode memory defaults to 0 if address not present
  if address < 0:
    raise newException(ValueError, "Invalid memory address: " & $address)
  mem.getOrDefault(address, 0)

proc getParamValue(state: var IntcodeState, paramIndex: int): int =
  let instruction = state.mem.getOrDefault(state.ip)
  let mode = (instruction div (10 ^ (paramIndex + 1))) mod 10
  let paramAddr = state.ip + paramIndex

  case mode
  of 0: # Position mode
    let address = state.mem.getOrDefault(paramAddr)
    return state.mem.getOrDefault(address)
  of 1: # Immediate mode
    return state.mem.getOrDefault(paramAddr)
  of 2: # Relative mode
    let address = state.mem.getOrDefault(paramAddr) + state.relativeBase
    return state.mem.getOrDefault(address)
  else:
    raise newException(ValueError, "Invalid parameter mode: " & $mode)

proc getParamAddress(state: var IntcodeState, paramIndex: int): int =
  let instruction = state.mem.getOrDefault(state.ip)
  let mode = (instruction div (10 ^ (paramIndex + 1))) mod 10
  let paramAddr = state.ip + paramIndex

  case mode
  of 0: # Position mode
    return state.mem.getOrDefault(paramAddr)
  of 2: # Relative mode
    return state.mem.getOrDefault(paramAddr) + state.relativeBase
  # Mode 1 (Immediate) is invalid for write addresses
  else:
    raise newException(ValueError, "Invalid parameter mode for write address: " & $mode)

proc runIntcode(state: var IntcodeState, inputs: var seq[int]) =
  state.awaitingInput = false # Reset flag if resuming
  state.output = @[]          # Clear previous output for this run

  while not state.halted and not state.awaitingInput:
    let instruction = state.mem.getOrDefault(state.ip)
    let opcode = instruction mod 100

    case opcode
    of 1: # Add
      let val1 = state.getParamValue(1)
      let val2 = state.getParamValue(2)
      let destAddr = state.getParamAddress(3)
      state.mem[destAddr] = val1 + val2
      state.ip += 4
    of 2: # Multiply
      let val1 = state.getParamValue(1)
      let val2 = state.getParamValue(2)
      let destAddr = state.getParamAddress(3)
      state.mem[destAddr] = val1 * val2
      state.ip += 4
    of 3: # Input
      if inputs.len == 0:
        state.awaitingInput = true # Pause execution
      else:
        let destAddr = state.getParamAddress(1)
        state.mem[destAddr] = inputs.pop() # Consume the first input
        state.ip += 2
    of 4: # Output
      let outVal = state.getParamValue(1)
      state.output.add(outVal)
      state.ip += 2
    of 5: # Jump-if-true
      let val1 = state.getParamValue(1)
      let val2 = state.getParamValue(2)
      if val1 != 0:
        state.ip = val2
      else:
        state.ip += 3
    of 6: # Jump-if-false
      let val1 = state.getParamValue(1)
      let val2 = state.getParamValue(2)
      if val1 == 0:
        state.ip = val2
      else:
        state.ip += 3
    of 7: # Less than
      let val1 = state.getParamValue(1)
      let val2 = state.getParamValue(2)
      let destAddr = state.getParamAddress(3)
      state.mem[destAddr] = if val1 < val2: 1 else: 0
      state.ip += 4
    of 8: # Equals
      let val1 = state.getParamValue(1)
      let val2 = state.getParamValue(2)
      let destAddr = state.getParamAddress(3)
      state.mem[destAddr] = if val1 == val2: 1 else: 0
      state.ip += 4
    of 9: # Adjust relative base
      let val1 = state.getParamValue(1)
      state.relativeBase += val1
      state.ip += 2
    of 99: # Halt
      state.halted = true
    else:
      raise newException(ValueError, "Invalid opcode: " & $opcode & " at ip " & $state.ip)

# --- Day 13 Logic ---

proc solve(): int =
  let programCode = readFile("input.txt").strip.split(',').map(parseInt)
  var state = initIntcodeState(programCode)
  var inputs: seq[int] = @[] # No input needed for Part 1

  runIntcode(state, inputs) # Run until halt or input required (should halt)

  if not state.halted:
    raise newException(Exception, "Intcode program did not halt as expected.")

  # Process the output
  var blockCount = 0
  var i = 0
  while i < state.output.len:
    # let x = state.output[i]     # Not needed for Part 1 count
    # let y = state.output[i+1]   # Not needed for Part 1 count
    let tileId = state.output[i+2]
    if tileId == 2: # Block tile
      blockCount.inc
    i += 3

  return blockCount

# --- Main Entry Point ---
when isMainModule:
  let result = solve()
  echo result

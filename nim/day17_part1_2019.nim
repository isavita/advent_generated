
import strutils, sequtils, tables, parseutils, os, streams

# Use int64 for Intcode compatibility
type Int = int64

# Intcode Computer State
type IntcodeComputer = object
  mem: Table[Int, Int] # Use Table for sparse memory, defaults to 0
  ip: Int
  relativeBase: Int
  input: seq[Int]
  output: seq[Int]
  halted: bool
  awaitingInput: bool
  inputPtr: int # To track consumption of input seq

# Initialize Intcode Computer from program code
proc initIntcodeComputer(programCode: seq[Int]): IntcodeComputer =
  result.mem = initTable[Int, Int]()
  for i, instruction in programCode:
    result.mem[int64(i)] = instruction
  result.ip = 0
  result.relativeBase = 0
  result.input = @[]
  result.output = @[]
  result.halted = false
  result.awaitingInput = false
  result.inputPtr = 0

# Get parameter value based on mode
proc getParam(computer: var IntcodeComputer, mode: int, offset: Int): Int =
  let paramAddr = computer.ip + offset
  let paramVal = computer.mem.getOrDefault(paramAddr)
  case mode
  of 0: # Position mode
    return computer.mem.getOrDefault(paramVal)
  of 1: # Immediate mode
    return paramVal
  of 2: # Relative mode
    return computer.mem.getOrDefault(computer.relativeBase + paramVal)
  else:
    raise newException(ValueError, "Invalid parameter mode: " & $mode)

# Get address for writing based on mode
proc getWriteAddr(computer: var IntcodeComputer, mode: int, offset: Int): Int =
  let paramAddr = computer.ip + offset
  let paramVal = computer.mem.getOrDefault(paramAddr)
  case mode
  of 0: # Position mode
    return paramVal
  of 2: # Relative mode
    return computer.relativeBase + paramVal
  # Mode 1 (Immediate) is invalid for write parameters
  else:
    raise newException(ValueError, "Invalid write parameter mode: " & $mode)

# Run the Intcode program
proc runIntcode(computer: var IntcodeComputer) =
  computer.halted = false
  computer.awaitingInput = false

  while not computer.halted and not computer.awaitingInput:
    let instruction = computer.mem.getOrDefault(computer.ip)
    let opcode = instruction mod 100
    let mode1 = (instruction div 100) mod 10
    let mode2 = (instruction div 1000) mod 10
    let mode3 = (instruction div 10000) mod 10

    case opcode
    of 1: # Add
      let val1 = computer.getParam(mode1, 1)
      let val2 = computer.getParam(mode2, 2)
      let destAddr = computer.getWriteAddr(mode3, 3)
      computer.mem[destAddr] = val1 + val2
      computer.ip += 4
    of 2: # Multiply
      let val1 = computer.getParam(mode1, 1)
      let val2 = computer.getParam(mode2, 2)
      let destAddr = computer.getWriteAddr(mode3, 3)
      computer.mem[destAddr] = val1 * val2
      computer.ip += 4
    of 3: # Input
      if computer.inputPtr >= computer.input.len:
        computer.awaitingInput = true # Pause execution
      else:
        let destAddr = computer.getWriteAddr(mode1, 1)
        computer.mem[destAddr] = computer.input[computer.inputPtr]
        computer.inputPtr += 1
        computer.ip += 2
        computer.awaitingInput = false # Ensure we are not awaiting if input was successful
    of 4: # Output
      let outputVal = computer.getParam(mode1, 1)
      computer.output.add(outputVal)
      computer.ip += 2
    of 5: # Jump-if-true
      let val1 = computer.getParam(mode1, 1)
      let val2 = computer.getParam(mode2, 2)
      if val1 != 0:
        computer.ip = val2
      else:
        computer.ip += 3
    of 6: # Jump-if-false
      let val1 = computer.getParam(mode1, 1)
      let val2 = computer.getParam(mode2, 2)
      if val1 == 0:
        computer.ip = val2
      else:
        computer.ip += 3
    of 7: # Less than
      let val1 = computer.getParam(mode1, 1)
      let val2 = computer.getParam(mode2, 2)
      let destAddr = computer.getWriteAddr(mode3, 3)
      computer.mem[destAddr] = if val1 < val2: 1 else: 0
      computer.ip += 4
    of 8: # Equals
      let val1 = computer.getParam(mode1, 1)
      let val2 = computer.getParam(mode2, 2)
      let destAddr = computer.getWriteAddr(mode3, 3)
      computer.mem[destAddr] = if val1 == val2: 1 else: 0
      computer.ip += 4
    of 9: # Adjust relative base
      let val1 = computer.getParam(mode1, 1)
      computer.relativeBase += val1
      computer.ip += 2
    of 99: # Halt
      computer.halted = true
    else:
      raise newException(ValueError, "Invalid opcode: " & $opcode & " at ip " & $computer.ip)

# --- Day 17 Specific Logic ---

proc solvePart1(programCode: seq[Int]): int =
  var computer = initIntcodeComputer(programCode)

  # Part 1 doesn't require input
  runIntcode(computer)

  if not computer.halted:
    echo "Error: Intcode program did not halt after initial run."
    quit(1)

  # Convert ASCII output to a map (grid)
  var grid: seq[string] = @[]
  var currentLine = ""
  for outputVal in computer.output:
    if outputVal == 10: # ASCII newline
      if currentLine.len > 0:
          grid.add(currentLine)
      currentLine = ""
    elif outputVal >= 0 and outputVal <= 127: # Valid ASCII range
      currentLine.add(char(outputVal))
    else:
      echo "Warning: Non-ASCII output value encountered: ", outputVal
      # Decide how to handle non-ASCII, maybe add placeholder or ignore
      # For this puzzle, it likely indicates an error or unexpected output
      # currentLine.add('?')

  # Add the last line if it wasn't terminated by a newline
  if currentLine.len > 0:
    grid.add(currentLine)

  if grid.len == 0:
     echo "Error: Generated grid is empty."
     quit(1)

  # Optional: Print the map for debugging
  # for row in grid:
  #   echo row

  let height = grid.len
  let width = grid[0].len # Assume rectangular grid

  var totalAlignmentParamSum = 0

  # Iterate through the grid, skipping borders, to find intersections
  for y in 1 ..< height - 1:
    for x in 1 ..< width - 1:
      # Check if current position is a scaffold '#'
      if grid[y][x] == '#':
        # Check all four neighbors are also scaffolds '#'
        if grid[y - 1][x] == '#' and
           grid[y + 1][x] == '#' and
           grid[y][x - 1] == '#' and
           grid[y][x + 1] == '#':
             # Found an intersection
             totalAlignmentParamSum += x * y

  return totalAlignmentParamSum

# Main execution block
when isMainModule:
  # Read program code from input.txt
  let inputFilePath = "input.txt"
  if not fileExists(inputFilePath):
    echo "Error: Input file '", inputFilePath, "' not found."
    quit(1)

  var programCode: seq[Int]
  try:
    let content = readFile(inputFilePath).strip()
    programCode = content.split(',').map(parseBiggestInt) # parseBiggestInt maps to int64
  except ValueError as e:
    echo "Error parsing input file: ", e.msg
    quit(1)
  except IOError as e:
     echo "Error reading input file: ", e.msg
     quit(1)

  # Calculate and print the result for Part 1
  let result = solvePart1(programCode)
  echo result

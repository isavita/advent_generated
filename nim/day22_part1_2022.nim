
import std/[strutils, sequtils, parseutils, math, os]

# --- Type Definitions ---
type
  Pos = tuple[r, c: int] # 0-based row, column
  Facing = range[0..3] # 0:R, 1:D, 2:L, 3:U
  RowBound = tuple[minCol, maxCol: int]
  ColBound = tuple[minRow, maxRow: int]

# --- Constants ---
const
  inputFile = "input.txt"
  dr = [0, 1, 0, -1] # R, D, L, U delta rows
  dc = [1, 0, -1, 0] # R, D, L, U delta cols

# --- Helper Procedures ---

proc isTile(map: seq[string], r, c: int): bool =
  ## Checks if (r, c) is within map bounds and is not an empty space ' '
  result = r >= 0 and r < map.len and
           c >= 0 and c < map[r].len and
           map[r][c] != ' '

proc turn(facing: var Facing, direction: char) =
  ## Turns the facing direction Left 'L' or Right 'R'
  if direction == 'R':
    facing = (facing + 1) mod 4
  elif direction == 'L':
    facing = (facing + 4 - 1) mod 4 # Add 4 to handle potential negative before modulo

proc move(map: seq[string], rowBounds: seq[RowBound], colBounds: seq[ColBound],
          pos: var Pos, facing: Facing, steps: int) =
  ## Moves the position 'steps' times according to facing, handling walls and wrapping.
  for _ in 1..steps:
    let currentR = pos.r
    let currentC = pos.c
    let nextRCheck = currentR + dr[facing]
    let nextCCheck = currentC + dc[facing]

    var targetR, targetC: int

    if isTile(map, nextRCheck, nextCCheck):
      # Next potential position is a valid tile on the map
      targetR = nextRCheck
      targetC = nextCCheck
    else:
      # Need to wrap around
      case facing
      of 0: # Right -> Wrap to Left edge
        targetR = currentR
        targetC = rowBounds[currentR].minCol
      of 1: # Down -> Wrap to Top edge
        targetR = colBounds[currentC].minRow
        targetC = currentC
      of 2: # Left -> Wrap to Right edge
        targetR = currentR
        targetC = rowBounds[currentR].maxCol
      of 3: # Up -> Wrap to Bottom edge
        targetR = colBounds[currentC].maxRow
        targetC = currentC

    # Check the tile at the target position (original or wrapped)
    if map[targetR][targetC] == '#':
      # Hit a wall, stop moving for this instruction
      break
    elif map[targetR][targetC] == '.':
      # Valid move, update position
      pos = (targetR, targetC)
    # else: Should not happen if isTile and wrapping find valid '.' or '#'

proc main() =
  # --- Input Reading ---
  let input = readFile(inputFile)
  let parts = input.split("\n\n")
  if parts.len != 2:
    stderr.writeLine "Error: Input file should have map and path separated by a blank line."
    quit(1)

  let mapLines = parts[0].splitLines()
  let pathStr = parts[1].strip()

  if mapLines.len == 0 or pathStr.len == 0:
    stderr.writeLine "Error: Map or path is empty."
    quit(1)

  # --- Preprocessing Map ---
  let numRows = mapLines.len
  let maxCols = mapLines.mapIt(it.len).max()

  # Calculate row and column bounds (for wrapping)
  var rowBounds = newSeq[RowBound](numRows)
  var colBounds = newSeq[ColBound](maxCols)

  # Initialize bounds with invalid values
  for r in 0..<numRows: rowBounds[r] = (maxCols, -1)
  for c in 0..<maxCols: colBounds[c] = (numRows, -1)

  # Find actual bounds
  for r in 0..<numRows:
    for c in 0..<mapLines[r].len:
      if mapLines[r][c] != ' ':
        rowBounds[r].minCol = min(rowBounds[r].minCol, c)
        rowBounds[r].maxCol = max(rowBounds[r].maxCol, c)
        colBounds[c].minRow = min(colBounds[c].minRow, r)
        colBounds[c].maxRow = max(colBounds[c].maxRow, r)

  # --- Initial State ---
  var currentPos: Pos
  # Find starting position: leftmost '.' in the first row
  try:
    currentPos = (0, mapLines[0].find('.'))
    if currentPos.c < 0: raise newException(ValueError, "No starting tile found")
  except ValueError:
    stderr.writeLine "Error: Could not find starting tile '.' in the first row."
    quit(1)

  var currentFacing: Facing = 0 # Start facing Right

  # --- Process Path ---
  var i = 0
  while i < pathStr.len:
    if pathStr[i].isDigit():
      var value = 0
      let parsed = parseInt(pathStr, value, i)
      if parsed == 0:
         stderr.writeLine "Error parsing number in path at index ", i
         quit(1)
      i += parsed
      # Perform move
      move(mapLines, rowBounds, colBounds, currentPos, currentFacing, value)
    else:
      # Perform turn
      turn(currentFacing, pathStr[i])
      i += 1

  # --- Calculate Final Password ---
  # Convert 0-based pos to 1-based row/col for the final answer
  let finalRow = currentPos.r + 1
  let finalCol = currentPos.c + 1
  let finalPassword = 1000 * finalRow + 4 * finalCol + currentFacing

  echo finalPassword

# --- Entry Point ---
when isMainModule:
  main()

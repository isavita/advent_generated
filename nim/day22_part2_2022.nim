
import os
import strutils
import re
import sequtils
import math

# --- Types ---
type
  Facing = enum
    Right = 0, Down = 1, Left = 2, Up = 3

  Pos = tuple[r, c: int] # 0-based row, col

  InstructionKind = enum
    Move, Turn

  Instruction = tuple[kind: InstructionKind, value: int] # value is steps for Move, +1 for R, -1 for L

# --- Constants ---
const dr = [0, 1, 0, -1] # R, D, L, U
const dc = [1, 0, -1, 0] # R, D, L, U

# --- Input Parsing ---
proc parseInput(filename: string): tuple[map: seq[string], path: string] =
  let content = readFile(filename)
  let parts = content.split("\n\n")
  if parts.len != 2:
    raise newException(ValueError, "Invalid input format: Expected map and path separated by double newline.")

  var mapLines = parts[0].splitLines()
  # Remove potential trailing empty line from splitLines
  if mapLines.len > 0 and mapLines[^1].len == 0:
    discard mapLines.pop()

  # Determine max width for potential padding (though we might not pad)
  var maxW = 0
  for line in mapLines:
    maxW = max(maxW, line.len)

  # Pad lines for easier indexing later? Let's try without padding first, using bounds checks.
  # for i in 0 ..< mapLines.len:
  #   mapLines[i] = mapLines[i] & repeat(' ', maxW - mapLines[i].len)

  return (map: mapLines, path: parts[1].strip())

proc parsePath(pathStr: string): seq[Instruction] =
  let tokens = pathStr.findAll(re"\d+|[LR]")
  result = @[]
  for token in tokens:
    if token[0].isDigit():
      result.add((kind: Move, value: parseInt(token)))
    elif token == "R":
      result.add((kind: Turn, value: 1)) # +1 for clockwise
    elif token == "L":
      result.add((kind: Turn, value: -1)) # -1 for counter-clockwise

# --- Part 1 Logic ---

# Helper to check if a position is valid on the map (not space, within bounds)
proc isValid(map: seq[string], r, c: int): bool =
  r >= 0 and r < map.len and c >= 0 and c < map[r].len and map[r][c] != ' '

# Find the start position (top-leftmost open tile)
proc findStart(map: seq[string]): Pos =
  for c in 0 ..< map[0].len:
    if map[0][c] == '.':
      return (r: 0, c: c)
  raise newException(ValueError, "Could not find starting position '.' in the first row.")

# Calculate the next position considering wrapping (Part 1)
proc getNextPosPart1(map: seq[string], currentPos: Pos, facing: Facing): Pos =
  var nextR = currentPos.r + dr[ord(facing)]
  var nextC = currentPos.c + dc[ord(facing)]

  if isValid(map, nextR, nextC):
    return (r: nextR, c: nextC)
  else:
    # Wrap around
    var wrapR = currentPos.r
    var wrapC = currentPos.c
    case facing:
    of Right: # Wrap to left
      wrapC = map[currentPos.r].find('.') # Find first tile (could be #)
      if wrapC == -1: wrapC = map[currentPos.r].find('#') # Should exist if row is valid
      if wrapC == -1 or map[currentPos.r][wrapC] == ' ': # Fallback, should not happen if row has non-space
         # This means we went right from a position where the wrap target is a space - need first non-space
         for scanC in 0 ..< map[currentPos.r].len:
            if map[currentPos.r][scanC] != ' ':
               wrapC = scanC
               break
    of Left: # Wrap to right
      wrapC = map[currentPos.r].rfind('.') # Find last tile
      if wrapC == -1: wrapC = map[currentPos.r].rfind('#')
      if wrapC == -1 or map[currentPos.r][wrapC] == ' ': # Fallback
         for scanC in countdown(map[currentPos.r].len - 1, 0):
             if map[currentPos.r][scanC] != ' ':
                 wrapC = scanC
                 break
    of Down: # Wrap to top
      for rScan in 0 ..< map.len:
        if currentPos.c < map[rScan].len and map[rScan][currentPos.c] != ' ':
          wrapR = rScan
          break
    of Up: # Wrap to bottom
      for rScan in countdown(map.len - 1, 0):
        if currentPos.c < map[rScan].len and map[rScan][currentPos.c] != ' ':
          wrapR = rScan
          break

    # If wrap target is still invalid (e.g., empty column), something is wrong
    # or the initial position itself was the only tile. Check the wrapped pos type.
    if wrapR == currentPos.r and wrapC == currentPos.c:
       # This case might happen if the map is very sparse, let's re-evaluate wrap logic
       # It should wrap to the first/last non-space character on the other side.
       # The logic above attempts this, let's assume it finds something.
       discard

    return (r: wrapR, c: wrapC)

proc solvePart1(map: seq[string], instructions: seq[Instruction]): int =
  var currentPos = findStart(map)
  var currentFacing = Right

  for instr in instructions:
    if instr.kind == Turn:
      let turnDir = instr.value
      currentFacing = Facing(((ord(currentFacing) + turnDir + 4) mod 4))
    else: # Move
      for _ in 1 .. instr.value:
        let nextPosCandidate = getNextPosPart1(map, currentPos, currentFacing)
        if isValid(map, nextPosCandidate.r, nextPosCandidate.c):
           if map[nextPosCandidate.r][nextPosCandidate.c] == '#':
             break # Hit a wall, stop this move instruction
           elif map[nextPosCandidate.r][nextPosCandidate.c] == '.':
             currentPos = nextPosCandidate # Move successful
           else:
             # Should not happen if getNextPosPart1 and isValid work correctly
             echo "Warning: Landed on unexpected tile type '", map[nextPosCandidate.r][nextPosCandidate.c], "' at ", nextPosCandidate
             break
        else:
            # This case indicates an issue with getNextPosPart1 logic if it returns an invalid pos
            echo "Error: getNextPosPart1 returned invalid position: ", nextPosCandidate
            break


  # Final password calculation (1-based indexing)
  return 1000 * (currentPos.r + 1) + 4 * (currentPos.c + 1) + ord(currentFacing)

# --- Part 2 Logic ---
# Assumes 50x50 face size for the specific input layout.
# Input layout visually (0-based coords, origin top-left):
#   Regions:    Coords (Top-Left) Face Size = 50 (F)
#      [1][2]   1: (0,  F)   2: (0, 2F)
#      [3]      3: ( F, F)
#   [4][5]      4: (2F, 0)   5: (2F, F)
#   [6]         6: (3F, 0)
const FaceSize = 50

# Determine which face a position belongs to
# Returns face number (1-6) or 0 if outside defined faces
proc getFace(pos: Pos): int =
    let r = pos.r
    let c = pos.c
    if r >= 0 and r < FaceSize: # Row 0-49
        if c >= FaceSize and c < 2*FaceSize: return 1
        if c >= 2*FaceSize and c < 3*FaceSize: return 2
    elif r >= FaceSize and r < 2*FaceSize: # Row 50-99
        if c >= FaceSize and c < 2*FaceSize: return 3
    elif r >= 2*FaceSize and r < 3*FaceSize: # Row 100-149
        if c >= 0 and c < FaceSize: return 4
        if c >= FaceSize and c < 2*FaceSize: return 5
    elif r >= 3*FaceSize and r < 4*FaceSize: # Row 150-199
        if c >= 0 and c < FaceSize: return 6
    return 0

# Calculate next position and facing for cube wrapping (HARDCODED FOR INPUT)
# Returns (newPos, newFacing)
proc getNextCubeWrap(pos: Pos, facing: Facing): tuple[nextPos: Pos, nextFacing: Facing] =
  let r = pos.r
  let c = pos.c

  # Relative coordinates within the current face (0 to FaceSize-1)
  let relR = r mod FaceSize
  let relC = c mod FaceSize

  # Determine transitions based on current face and exit direction
  case facing:
  of Right: # Moving Right
    if r >= 0 and r < FaceSize and c == 3*FaceSize - 1: # Face 2 -> Face 5 (flipped)
      result = (nextPos: (2*FaceSize + (FaceSize - 1 - relR), 2*FaceSize - 1), nextFacing: Left)
    elif r >= FaceSize and r < 2*FaceSize and c == 2*FaceSize - 1: # Face 3 -> Face 2
      result = (nextPos: (FaceSize - 1, 2*FaceSize + relR), nextFacing: Up)
    elif r >= 2*FaceSize and r < 3*FaceSize and c == 2*FaceSize - 1: # Face 5 -> Face 2 (flipped)
      result = (nextPos: (FaceSize - 1 - relR, 3*FaceSize - 1), nextFacing: Left)
    elif r >= 3*FaceSize and r < 4*FaceSize and c == FaceSize - 1: # Face 6 -> Face 5
      result = (nextPos: (3*FaceSize - 1, FaceSize + relR), nextFacing: Up)
    else: raise newException(ValueError, "Invalid Right wrap state")
  of Down: # Moving Down
    if r == FaceSize - 1 and c >= 2*FaceSize and c < 3*FaceSize: # Face 2 -> Face 3
      result = (nextPos: (FaceSize + relC, 2*FaceSize - 1), nextFacing: Left)
    elif r == 3*FaceSize - 1 and c >= FaceSize and c < 2*FaceSize: # Face 5 -> Face 6
      result = (nextPos: (3*FaceSize + relC, FaceSize - 1), nextFacing: Left)
    elif r == 4*FaceSize - 1 and c >= 0 and c < FaceSize: # Face 6 -> Face 2
       result = (nextPos: (0, 2*FaceSize + relC), nextFacing: Down) # Facing stays Down! Mistake in manual derivation earlier? Check again. Yes, simple fold, Down->Down.
    else: raise newException(ValueError, "Invalid Down wrap state")
  of Left: # Moving Left
    if r >= 0 and r < FaceSize and c == FaceSize: # Face 1 -> Face 4 (flipped)
      result = (nextPos: (2*FaceSize + (FaceSize - 1 - relR), 0), nextFacing: Right)
    elif r >= FaceSize and r < 2*FaceSize and c == FaceSize: # Face 3 -> Face 4
      result = (nextPos: (2*FaceSize, relR), nextFacing: Down)
    elif r >= 2*FaceSize and r < 3*FaceSize and c == 0: # Face 4 -> Face 1 (flipped)
      result = (nextPos: (FaceSize - 1 - relR, FaceSize), nextFacing: Right)
    elif r >= 3*FaceSize and r < 4*FaceSize and c == 0: # Face 6 -> Face 1
       result = (nextPos: (0, FaceSize + relR), nextFacing: Down)
    else: raise newException(ValueError, "Invalid Left wrap state")
  of Up: # Moving Up
    if r == 0 and c >= FaceSize and c < 2*FaceSize: # Face 1 -> Face 6
      result = (nextPos: (3*FaceSize + relC, 0), nextFacing: Right)
    elif r == 0 and c >= 2*FaceSize and c < 3*FaceSize: # Face 2 -> Face 6
      result = (nextPos: (4*FaceSize - 1, relC), nextFacing: Up) # Simple fold, Up -> Up
    elif r == 2*FaceSize and c >= 0 and c < FaceSize: # Face 4 -> Face 3
       result = (nextPos: (FaceSize + relC, FaceSize), nextFacing: Right)
    else: raise newException(ValueError, "Invalid Up wrap state")


proc solvePart2(map: seq[string], instructions: seq[Instruction]): int =
  var currentPos = findStart(map)
  var currentFacing = Right

  for instr in instructions:
    if instr.kind == Turn:
      let turnDir = instr.value
      currentFacing = Facing(((ord(currentFacing) + turnDir + 4) mod 4))
    else: # Move
      for _ in 1 .. instr.value:
        var nextR = currentPos.r + dr[ord(currentFacing)]
        var nextC = currentPos.c + dc[ord(currentFacing)]
        var nextFacing = currentFacing

        var potentialNextPos: Pos
        var potentialNextFacing: Facing

        if isValid(map, nextR, nextC):
          potentialNextPos = (r: nextR, c: nextC)
          potentialNextFacing = currentFacing
        else:
          # Need to wrap using cube logic
          (potentialNextPos, potentialNextFacing) = getNextCubeWrap(currentPos, currentFacing)

        # Check if the potential next position (after normal move or wrap) is a wall
        if isValid(map, potentialNextPos.r, potentialNextPos.c):
          if map[potentialNextPos.r][potentialNextPos.c] == '#':
            break # Hit a wall, stop this move instruction
          elif map[potentialNextPos.r][potentialNextPos.c] == '.':
            # Move successful
            currentPos = potentialNextPos
            currentFacing = potentialNextFacing # Update facing only if wrap occurred
          else:
             # Should not happen on valid map spots
             echo "Warning: Cube logic landed on unexpected tile type '", map[potentialNextPos.r][potentialNextPos.c], "' at ", potentialNextPos
             break
        else:
           echo "Error: Cube wrap resulted in invalid position: ", potentialNextPos, " from ", currentPos, " facing ", currentFacing
           break


  # Final password calculation (1-based indexing)
  return 1000 * (currentPos.r + 1) + 4 * (currentPos.c + 1) + ord(currentFacing)

# --- Main Execution ---
proc main() =
  let filename = "input.txt"
  if not fileExists(filename):
    echo "Error: Input file '", filename, "' not found."
    quit(1)

  try:
    let (map, pathStr) = parseInput(filename)
    let instructions = parsePath(pathStr)

    # --- Part 1 ---
    let answer1 = solvePart1(map, instructions)
    echo "Part 1: ", answer1

    # --- Part 2 ---
    let answer2 = solvePart2(map, instructions)
    echo "Part 2: ", answer2

  except ValueError as e:
    echo "Input Error: ", e.msg
    quit(1)
  except Exception as e:
    echo "An unexpected error occurred: ", e.msg
    quit(1)


when isMainModule:
  main()

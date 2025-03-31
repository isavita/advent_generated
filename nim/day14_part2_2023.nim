
import std/[strutils, sequtils, tables, hashes]

# --- Data Structures ---
type Grid = seq[string]

# --- Helper Functions ---

proc hashGrid(grid: Grid): Hash =
  ## Calculates a hash for the grid state.
  result = 0
  for row in grid:
    result = result !& hash(row)

proc calculateLoad(grid: Grid): int =
  ## Calculates the total load on the north support beams.
  let H = grid.len
  result = 0
  for r, row in grid:
    for c, cell in row:
      if cell == 'O':
        result += H - r

proc tiltNorth(grid: var Grid) =
  ## Tilts the grid north, moving 'O' rocks up.
  let H = grid.len
  let W = grid[0].len
  for c in 0 ..< W:
    var landingRow = 0
    for r in 0 ..< H:
      case grid[r][c]
      of '#':
        landingRow = r + 1
      of 'O':
        # Move the rock
        grid[r][c] = '.' # Erase original position first
        grid[landingRow][c] = 'O'
        inc landingRow
      else: # '.'
        discard # Do nothing, just continue scanning

proc tiltWest(grid: var Grid) =
  ## Tilts the grid west, moving 'O' rocks left.
  let H = grid.len
  let W = grid[0].len
  for r in 0 ..< H:
    var landingCol = 0
    for c in 0 ..< W:
      case grid[r][c]
      of '#':
        landingCol = c + 1
      of 'O':
        # Move the rock
        grid[r][c] = '.' # Erase original position first
        grid[r][landingCol] = 'O'
        inc landingCol
      else: # '.'
        discard

proc tiltSouth(grid: var Grid) =
  ## Tilts the grid south, moving 'O' rocks down.
  let H = grid.len
  let W = grid[0].len
  for c in 0 ..< W:
    var landingRow = H - 1
    for r in countdown(H - 1, 0): # Iterate bottom-up
      case grid[r][c]
      of '#':
        landingRow = r - 1
      of 'O':
        # Move the rock
        grid[r][c] = '.' # Erase original position first
        grid[landingRow][c] = 'O'
        dec landingRow
      else: # '.'
        discard

proc tiltEast(grid: var Grid) =
  ## Tilts the grid east, moving 'O' rocks right.
  let H = grid.len
  let W = grid[0].len
  for r in 0 ..< H:
    var landingCol = W - 1
    for c in countdown(W - 1, 0): # Iterate right-to-left
      case grid[r][c]
      of '#':
        landingCol = c - 1
      of 'O':
        # Move the rock
        grid[r][c] = '.' # Erase original position first
        grid[r][landingCol] = 'O'
        dec landingCol
      else: # '.'
        discard

proc spinCycle(grid: var Grid) =
  ## Performs one full spin cycle: North, West, South, East.
  tiltNorth(grid)
  tiltWest(grid)
  tiltSouth(grid)
  tiltEast(grid)

# --- Main Logic ---
when isMainModule:
  let inputLines = readFile("input.txt").strip.splitLines()
  var gridPart1 = inputLines # Make a copy for Part 1
  var gridPart2 = inputLines # Make a copy for Part 2

  # --- Part 1 ---
  tiltNorth(gridPart1)
  let loadPart1 = calculateLoad(gridPart1)
  echo "Part 1: ", loadPart1

  # --- Part 2 ---
  const totalCycles = 1_000_000_000
  var seenStates = initOrderedTable[Hash, int]() # Use OrderedTable to preserve insertion order if needed, though Table works too
  var cycleNum = 0
  var cycleFound = false
  var finalLoadPart2 = 0

  while cycleNum < totalCycles:
    inc cycleNum
    spinCycle(gridPart2)
    let currentHash = hashGrid(gridPart2)

    if currentHash in seenStates:
      let firstSeenCycle = seenStates[currentHash]
      let cycleLength = cycleNum - firstSeenCycle
      let remainingCycles = totalCycles - cycleNum
      let skipCycles = (remainingCycles div cycleLength) * cycleLength
      cycleNum += skipCycles
      # We don't need to simulate the skipped cycles, just advance cycleNum
      # The grid is already at the state corresponding to `cycleNum` mod `cycleLength` offset
      # within the detected cycle. We just need to continue the loop until cycleNum reaches totalCycles.
      # Clear seenStates to prevent re-detecting the same cycle repeatedly in the remaining steps.
      clear(seenStates) # Avoid re-entering this block
      cycleFound = true
      # echo "Cycle detected at step ", cycleNum, "! First seen: ", firstSeenCycle, " Length: ", cycleLength
      # echo "Skipping ", skipCycles, " cycles."

    elif not cycleFound: # Only store if we haven't found the cycle yet
        seenStates[currentHash] = cycleNum

  # After the loop (either completed or fast-forwarded using cycle detection)
  finalLoadPart2 = calculateLoad(gridPart2)
  echo "Part 2: ", finalLoadPart2

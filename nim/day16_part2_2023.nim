
import std/[strutils, sequtils, sets, deques, streams, os]

# --- Types and Constants ---

type
  Direction* = enum
    Up, Down, Left, Right

  Coord* = tuple[r, c: int]

  BeamState* = tuple[pos: Coord, dir: Direction]

# Precomputed offsets for each direction (dr, dc)
const DirOffsets: array[Direction, Coord] = [
  Up:    (r: -1, c:  0),
  Down:  (r:  1, c:  0),
  Left:  (r:  0, c: -1),
  Right: (r:  0, c:  1)
]

# --- Core Simulation Logic ---

proc simulateBeam(grid: seq[string], startBeam: BeamState): int =
  ## Simulates the beam path starting from `startBeam` and returns the number of energized tiles.

  let rows = grid.len
  let cols = if rows > 0: grid[0].len else: 0
  if rows == 0 or cols == 0: return 0 # Handle empty grid

  var energizedTiles: HashSet[Coord]
  var visitedStates: HashSet[BeamState]
  var queue: Deque[BeamState]

  queue.addLast(startBeam)

  while queue.len > 0:
    let currentBeam = queue.popFirst()
    let (pos, dir) = currentBeam

    # 1. Boundary Check
    if pos.r < 0 or pos.r >= rows or pos.c < 0 or pos.c >= cols:
      continue

    # 2. Cycle Check: Have we been in this exact state (position + direction) before?
    if currentBeam in visitedStates:
      continue
    visitedStates.incl(currentBeam)

    # 3. Mark Tile Energized (only need position)
    energizedTiles.incl(pos)

    # 4. Process Tile and Determine Next Beam(s)
    let tile = grid[pos.r][pos.c]
    var nextDirs: seq[Direction] = @[]

    case tile
    of '.':
      # Continue in the same direction
      nextDirs.add(dir)
    of '/':
      # Reflect 90 degrees
      case dir
      of Right: nextDirs.add(Up)
      of Left:  nextDirs.add(Down)
      of Up:    nextDirs.add(Right)
      of Down:  nextDirs.add(Left)
    of '\\': # Use double backslash for literal character
      # Reflect 90 degrees
      case dir
      of Right: nextDirs.add(Down)
      of Left:  nextDirs.add(Up)
      of Up:    nextDirs.add(Left)
      of Down:  nextDirs.add(Right)
    of '|':
      # Split or pass through
      if dir == Left or dir == Right:
        # Split vertically
        nextDirs.add(Up)
        nextDirs.add(Down)
      else:
        # Pass through vertically
        nextDirs.add(dir)
    of '-':
      # Split or pass through
      if dir == Up or dir == Down:
        # Split horizontally
        nextDirs.add(Left)
        nextDirs.add(Right)
      else:
        # Pass through horizontally
        nextDirs.add(dir)
    else: discard # Should not happen with valid input

    # 5. Enqueue Next State(s)
    for nextDir in nextDirs:
      let offset = DirOffsets[nextDir]
      let nextPos = (r: pos.r + offset.r, c: pos.c + offset.c)
      queue.addLast((pos: nextPos, dir: nextDir))

  return energizedTiles.len

# --- Main Program Logic ---

proc solve(): (int, int) =
  ## Reads input, solves both parts, and returns the results.
  let inputStream = open("input.txt", fmRead)
  defer: inputStream.close()
  let grid = inputStream.readAll().strip().splitLines()

  if grid.len == 0:
    quit("Error: Input file is empty or invalid.")

  let rows = grid.len
  let cols = grid[0].len

  # Part 1: Start at top-left, heading right
  let part1Result = simulateBeam(grid, (pos: (r: 0, c: 0), dir: Right))

  # Part 2: Try all edge starting positions
  var maxEnergized = 0

  # Top row (heading Down)
  for c in 0..<cols:
    maxEnergized = max(maxEnergized, simulateBeam(grid, (pos: (r: 0, c: c), dir: Down)))

  # Bottom row (heading Up)
  for c in 0..<cols:
    maxEnergized = max(maxEnergized, simulateBeam(grid, (pos: (r: rows - 1, c: c), dir: Up)))

  # Left column (heading Right)
  for r in 0..<rows:
    maxEnergized = max(maxEnergized, simulateBeam(grid, (pos: (r: r, c: 0), dir: Right)))

  # Right column (heading Left)
  for r in 0..<rows:
    maxEnergized = max(maxEnergized, simulateBeam(grid, (pos: (r: r, c: cols - 1), dir: Left)))

  return (part1Result, maxEnergized)

# --- Entry Point ---

when isMainModule:
  let (part1, part2) = solve()
  echo "Part 1: ", part1
  echo "Part 2: ", part2

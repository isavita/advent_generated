
import std/[strutils, sequtils, sets, deques, os]

# --- Type Definitions ---

# Represents a point on the grid
type Point = tuple[r, c: int]

# Represents a direction of movement (delta row, delta column)
type Direction = tuple[dr, dc: int]

# Represents the state of a beam: its position and direction
type BeamState = tuple[pos: Point, dir: Direction]

# --- Core Simulation Logic ---

proc solve(grid: seq[string]): int =
  # Handle empty grid case
  if grid.len == 0 or grid[0].len == 0:
    return 0

  let rows = grid.len
  let cols = grid[0].len

  # Queue for Breadth-First Search (BFS) of beam states
  var q = initDeque[BeamState]()
  # Set to track visited states (position + direction) to prevent cycles
  var visitedStates = initHashSet[BeamState]()
  # Set to track unique energized tile positions
  var energizedTiles = initHashSet[Point]()

  # Initial beam: starts at top-left (0, 0), moving right (0, 1)
  let startBeam: BeamState = (pos: (r: 0, c: 0), dir: (dr: 0, dc: 1))
  q.addLast(startBeam)

  while q.len > 0:
    let currentBeam = q.popFirst()
    let (r, c) = currentBeam.pos
    let (dr, dc) = currentBeam.dir

    # --- Boundary Check ---
    # If beam is outside the grid, stop processing it
    if r < 0 or r >= rows or c < 0 or c >= cols:
      continue

    # --- Cycle Check ---
    # If this exact state (position + direction) has been visited,
    # we don't need to process it again.
    # `containsOrIncl` checks if it exists and adds it if not, returning true if it already existed.
    if visitedStates.containsOrIncl(currentBeam):
       continue # Skip if already visited in this state

    # --- Mark Tile Energized ---
    # Add the current position to the set of energized tiles
    energizedTiles.incl(currentBeam.pos)

    # --- Process Tile Interaction ---
    let tile = grid[r][c]
    var nextDirs: seq[Direction] = @[] # Sequence to hold the direction(s) for the next step(s)

    case tile
    of '.':
      # Empty space: Continue in the same direction
      nextDirs.add(currentBeam.dir)
    of '/':
      # Mirror /: Reflects 90 degrees
      # (dr, dc) -> (-dc, -dr)
      # R(0,1)->U(-1,0); L(0,-1)->D(1,0); U(-1,0)->R(0,1); D(1,0)->L(0,-1)
      nextDirs.add((-dc, -dr))
    of '\\': # Backslash needs escaping
      # Mirror \: Reflects 90 degrees
      # (dr, dc) -> (dc, dr)
      # R(0,1)->D(1,0); L(0,-1)->U(-1,0); U(-1,0)->L(0,-1); D(1,0)->R(0,1)
      nextDirs.add((dc, dr))
    of '|':
      # Splitter |
      if dr == 0: # Moving horizontally (Left or Right)
        # Split into Up and Down
        nextDirs.add((-1, 0)) # Up
        nextDirs.add((1, 0))  # Down
      else: # Moving vertically (Up or Down)
        # Pass through
        nextDirs.add(currentBeam.dir)
    of '-':
      # Splitter -
      if dc == 0: # Moving vertically (Up or Down)
        # Split into Left and Right
        nextDirs.add((0, -1)) # Left
        nextDirs.add((0, 1))  # Right
      else: # Moving horizontally (Left or Right)
        # Pass through
        nextDirs.add(currentBeam.dir)
    else:
      # Should not happen with valid input, but good practice to handle
      raise newException(ValueError, "Invalid character in grid: " & $tile)

    # --- Enqueue Next Beam States ---
    for nextDir in nextDirs:
      # Calculate the next position based on the new direction
      let nextPos: Point = (r: r + nextDir.dr, c: c + nextDir.dc)
      # Add the new beam state(s) to the queue for processing
      q.addLast((pos: nextPos, dir: nextDir))

  # The result is the total number of unique energized tiles
  return energizedTiles.len

# --- Main Entry Point ---

proc main() =
  # Define the input filename
  let filename = "input.txt"

  # Check if the input file exists
  if not fileExists(filename):
    echo "Error: Input file '" & filename & "' not found."
    quit(1) # Exit with an error code

  # Read the grid layout from the file
  # `strip` removes leading/trailing whitespace, `splitLines` creates seq[string]
  let grid = readFile(filename).strip.splitLines()

  # Calculate the number of energized tiles
  let energizedCount = solve(grid)

  # Print the result to standard output
  echo energizedCount

# Execute the main procedure when the script is run
when isMainModule:
  main()

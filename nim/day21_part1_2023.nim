
import std/os
import std/strutils
import std/sequtils
import std/sets
import std/tables # For potential optimizations later, though HashSet is fine here

# --- Constants ---
const
  InputFileName = "input.txt"
  TargetSteps = 64
  GardenPlot = '.'
  Rock = '#'
  Start = 'S'

# --- Types ---
type
  Coord = tuple[r, c: int]
  Grid = seq[string]

# --- Helper Procedures ---

proc `+`(a, b: Coord): Coord =
  ## Adds two coordinates component-wise.
  result = (a.r + b.r, a.c + b.c)

proc isValid(grid: Grid, pos: Coord): bool =
  ## Checks if a coordinate is within the grid boundaries.
  let numRows = grid.len
  let numCols = if numRows > 0: grid[0].len else: 0
  result = pos.r >= 0 and pos.r < numRows and
           pos.c >= 0 and pos.c < numCols

proc isWalkable(grid: Grid, pos: Coord): bool =
  ## Checks if a coordinate is within bounds and not a rock.
  result = isValid(grid, pos) and grid[pos.r][pos.c] != Rock

proc findStart(grid: Grid): Coord =
  ## Finds the starting 'S' position in the grid.
  for r, rowStr in grid:
    for c, cell in rowStr:
      if cell == Start:
        return (r, c)
  # Should not happen with valid input, but good practice to handle
  raise newException(ValueError, "Starting position 'S' not found in grid.")

# --- Main Logic ---

proc solve(grid: var Grid, startPos: Coord, steps: int): int =
  ## Performs a Breadth-First Search (BFS) style simulation step by step.
  ## Returns the number of reachable plots after exactly 'steps'.

  # Treat the starting position as a regular garden plot for movement
  grid[startPos.r][startPos.c] = GardenPlot

  let directions: array[4, Coord] = [(0, 1), (0, -1), (1, 0), (-1, 0)] # E, W, S, N

  # Use a HashSet to store unique reachable coordinates at the current step
  var currentPositions: HashSet[Coord]
  currentPositions.incl(startPos)

  # Simulate step by step
  for step in 1..steps:
    var nextPositions: HashSet[Coord]
    for pos in currentPositions:
      for dir in directions:
        let nextPos = pos + dir
        if isWalkable(grid, nextPos):
          nextPositions.incl(nextPos)
    # Update the set of current positions for the next iteration
    currentPositions = nextPositions
    # Optimization: If the set becomes empty, no further steps are possible
    if currentPositions.len == 0:
      break

  # The result is the number of unique positions reachable at the final step
  result = currentPositions.len

# --- Entry Point ---

proc main() =
  # 1. Read Input
  if not fileExists(InputFileName):
    echo "Error: Input file '", InputFileName, "' not found."
    quit(QuitFailure)

  let input = readFile(InputFileName)
  var grid = input.strip().splitLines()

  if grid.len == 0 or grid[0].len == 0:
    echo "Error: Input grid is empty."
    quit(QuitFailure)

  # 2. Find Start and Prepare Grid
  var startPos: Coord
  try:
    startPos = findStart(grid)
  except ValueError as e:
    echo e.msg
    quit(QuitFailure)

  # 3. Solve the problem
  let reachablePlots = solve(grid, startPos, TargetSteps)

  # 4. Print Output
  echo reachablePlots

# --- Run ---
when isMainModule:
  main()

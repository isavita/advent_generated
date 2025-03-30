
import std/[sequtils, strutils, sets, deques, os]

# --- Type Definitions ---
type
  Coord = tuple[r, c: int] # Represents a coordinate (row, column)
  Grid = seq[seq[int]]     # Represents the topographic map as a 2D sequence of integers

# --- Helper Functions ---

# Check if a coordinate is within the grid boundaries
func isValid(r, c, rows, cols: int): bool {.inline.} =
  r >= 0 and r < rows and c >= 0 and c < cols

# Perform Breadth-First Search (BFS) from a starting coordinate (trailhead)
# to find all reachable '9's following the hiking trail rules.
func findReachableNines(grid: Grid, startCoord: Coord): HashSet[Coord] =
  let rows = grid.len
  let cols = grid[0].len

  # Queue for BFS, storing coordinates to visit
  var q = initDeque[Coord]()
  # Set to keep track of visited coordinates *during this specific BFS*
  # to avoid cycles and redundant exploration within this trail search.
  var visited = initHashSet[Coord]()
  # Set to store the coordinates of reachable '9's from this trailhead
  var reachedNines = initHashSet[Coord]()

  # Initialize BFS
  q.addLast(startCoord)
  visited.incl(startCoord)

  while q.len > 0:
    let currentCoord = q.popFirst()
    let (r, c) = currentCoord
    let currentHeight = grid[r][c]

    # If we reached a '9', add it to the set for this trailhead
    if currentHeight == 9:
      reachedNines.incl(currentCoord)
      # Optimization: A path ends at 9, no need to explore further *from* this 9
      # based on the rule height[next] == height[current] + 1.
      # The BFS naturally handles this as no neighbour will satisfy the condition.

    # Explore neighbors (Up, Down, Left, Right)
    # Define relative movements for neighbours
    let dr = [-1, 1, 0, 0] # Delta row
    let dc = [0, 0, -1, 1] # Delta column

    for i in 0..<4:
      let nextR = r + dr[i]
      let nextC = c + dc[i]
      let nextCoord = (r: nextR, c: nextC)

      # Check if the neighbor is valid (within bounds)
      if isValid(nextR, nextC, rows, cols):
        let nextHeight = grid[nextR][nextC]
        # Check the hiking trail rule: height increases by exactly 1
        # and the neighbor hasn't been visited in *this* BFS run.
        if nextHeight == currentHeight + 1 and nextCoord notin visited:
          visited.incl(nextCoord)
          q.addLast(nextCoord)

  return reachedNines

# --- Main Program Logic ---
proc main() =
  # 1. Read Input Grid from "input.txt"
  var grid: Grid = @[]
  try:
    for line in lines("input.txt"):
      let trimmedLine = line.strip()
      if trimmedLine.len > 0:
        # Convert each character digit to an integer
        grid.add(trimmedLine.map(proc(c: char): int = c.ord - '0'.ord))
  except IOError:
    echo "Error: Could not read input file 'input.txt'."
    quit(1)
  except ValueError:
     echo "Error: Input file contains non-digit characters."
     quit(1)


  if grid.len == 0 or grid[0].len == 0:
    echo "Error: Input grid is empty or invalid."
    quit(1)

  let rows = grid.len
  let cols = grid[0].len

  # 2. Find all Trailheads (coordinates with height 0)
  var trailheads: seq[Coord] = @[]
  for r in 0..<rows:
    for c in 0..<cols:
      if grid[r][c] == 0:
        trailheads.add((r: r, c: c))

  # 3. Calculate Total Score
  var totalScore = 0
  for trailhead in trailheads:
    # For each trailhead, find all unique '9's reachable via valid hiking trails
    let reachableNines = findReachableNines(grid, trailhead)
    # The score for this trailhead is the count of unique reachable '9's
    totalScore += reachableNines.len

  # 4. Print the final summed score
  echo totalScore

# --- Program Entry Point ---
when isMainModule:
  main()

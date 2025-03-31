
# Compile with: nim c -d:release --opt:speed <filename>.nim
# Reads from input.txt and prints to stdout.

import std/[strutils, sequtils, os, math]
import collections/deques

const InputFile = "input.txt"

# Represents a coordinate (row, column) in the grid
type Coord = tuple[row, col: int]

# Check if a coordinate is within the grid boundaries
proc isValid(pos: Coord, R, C: int): bool {.inline.} =
  # Inline for potentially frequent calls
  result = pos.row >= 0 and pos.row < R and pos.col >= 0 and pos.col < C

# Get the two coordinates potentially connected by a given pipe character at 'pos'.
# This function returns the two connected coordinates based *only* on the pipe type.
# It does not validate if these coordinates are within bounds or if the pipes there connect back.
proc getPotentialNeighbors(pipe: char, pos: Coord): array[2, Coord] =
  let (r, c) = pos
  case pipe
  of '|': result = [(r - 1, c).Coord, (r + 1, c).Coord] # North, South
  of '-': result = [(r, c - 1).Coord, (r, c + 1).Coord] # West, East
  of 'L': result = [(r - 1, c).Coord, (r, c + 1).Coord] # North, East
  of 'J': result = [(r - 1, c).Coord, (r, c - 1).Coord] # North, West
  of '7': result = [(r + 1, c).Coord, (r, c - 1).Coord] # South, West
  of 'F': result = [(r + 1, c).Coord, (r, c + 1).Coord] # South, East
  else:   result = [(-1, -1).Coord, (-1, -1).Coord] # '.' or 'S' (S handled separately initially) or invalid char

# Main program logic
proc main() =
  # Read Input File with basic error handling
  let fileContent = try: readFile(InputFile)
                    except IOError: "" # Return empty string on error

  if fileContent.len == 0:
    stderr.writeLine "Error: Could not read file or file is empty: ", InputFile
    quit(1) # Exit with a non-zero code to indicate failure

  let inputGrid = fileContent.strip.splitLines() # Split into lines, remove leading/trailing whitespace
  let R = inputGrid.len # Number of rows
  let C = if R > 0: inputGrid[0].len else: 0 # Number of columns
  if R == 0 or C == 0:
    stderr.writeLine "Error: Empty input grid."
    quit(1)

  # Find Starting Position 'S'
  var startPos: Coord = (-1, -1) # Initialize with invalid coordinates
  block findStart: # Use a block for early exit
    for r in 0..<R:
      # Minor optimization: access row string once per row iteration
      let rowStr = inputGrid[r]
      for c in 0..<C:
        if rowStr[c] == 'S':
          startPos = (r, c)
          break findStart # Exit block immediately once 'S' is found

  if startPos.row == -1: # Check if 'S' was found
    stderr.writeLine "Error: Start 'S' not found in the grid."
    quit(1)

  # Initialize distances grid. -1 indicates the cell is unvisited.
  # newSeqWith is efficient for creating nested sequences with default values.
  var distances = newSeqWith(R, newSeq[int](C))
  for r in 0..<R:
    for c in 0..<C:
      distances[r][c] = -1 # Use -1 as a sentinel value for unvisited cells

  # Initialize Breadth-First Search (BFS)
  var q = initDeque[Coord]() # Use a Deque as an efficient queue
  distances[startPos.row][startPos.col] = 0 # Distance from start to itself is 0

  # --- Find initial valid neighbors of 'S' and add them to the queue ---
  # A neighbor is valid if it's within bounds, is a pipe character,
  # and that pipe character logically connects back towards 'S'.
  let (sr, sc) = startPos
  # Check North neighbor
  if sr > 0: # Check boundary
    let nextPos: Coord = (sr - 1, sc)
    let pipe = inputGrid[nextPos.row][nextPos.col]
    # Check if the North pipe type connects South (towards 'S')
    if pipe in {'|', '7', 'F'}:
       distances[nextPos.row][nextPos.col] = 1 # Set distance to 1
       q.addLast(nextPos)                   # Add to BFS queue
  # Check South neighbor
  if sr < R - 1: # Check boundary
    let nextPos: Coord = (sr + 1, sc)
    let pipe = inputGrid[nextPos.row][nextPos.col]
    # Check if the South pipe type connects North (towards 'S')
    if pipe in {'|', 'L', 'J'}:
       # Check if already visited (could happen if S is a corner/pipe itself)
       if distances[nextPos.row][nextPos.col] == -1:
         distances[nextPos.row][nextPos.col] = 1
         q.addLast(nextPos)
  # Check West neighbor
  if sc > 0: # Check boundary
    let nextPos: Coord = (sr, sc - 1)
    let pipe = inputGrid[nextPos.row][nextPos.col]
    # Check if the West pipe type connects East (towards 'S')
    if pipe in {'-', 'L', 'F'}:
       if distances[nextPos.row][nextPos.col] == -1:
         distances[nextPos.row][nextPos.col] = 1
         q.addLast(nextPos)
  # Check East neighbor
  if sc < C - 1: # Check boundary
    let nextPos: Coord = (sr, sc + 1)
    let pipe = inputGrid[nextPos.row][nextPos.col]
    # Check if the East pipe type connects West (towards 'S')
    if pipe in {'-', 'J', '7'}:
       if distances[nextPos.row][nextPos.col] == -1:
         distances[nextPos.row][nextPos.col] = 1
         q.addLast(nextPos)

  # --- Perform BFS to find distances along the pipe loop ---
  var maxDist = 0 # Keep track of the maximum distance encountered
  while q.len > 0: # While there are cells to visit
    let currPos = q.popFirst() # Get the next cell from the front of the queue
    let currDist = distances[currPos.row][currPos.col]

    # Update the maximum distance found so far. Because BFS explores layer by layer,
    # the last distance recorded will be close to the maximum, but tracking max ensures correctness.
    maxDist = max(maxDist, currDist)

    let currPipeChar = inputGrid[currPos.row][currPos.col]
    # Get the two positions this pipe segment potentially connects to
    let potentialNextCoords = getPotentialNeighbors(currPipeChar, currPos)

    for nextPos in potentialNextCoords:
        # Check if the potential neighbor is:
        # 1. Within grid bounds using the isValid helper
        # 2. Has not been visited yet (distance == -1)
        if isValid(nextPos, R, C) and distances[nextPos.row][nextPos.col] == -1:
            # 3. Is not ground ('.'). This ensures we stay on the pipe loop.
            #    We don't need to check for 'S' here, as S is distance 0 and already visited.
            if inputGrid[nextPos.row][nextPos.col] != '.':
                # If valid, unvisited, and part of the pipe network, update distance and enqueue
                distances[nextPos.row][nextPos.col] = currDist + 1
                q.addLast(nextPos)

  # The maximum distance found during the BFS traversal represents the farthest point
  # from 'S' along the loop.
  echo maxDist

# Execute the main procedure
main()


import sequtils
import strutils
import os

const Directions = [(0, 1), (0, -1), (1, 0), (-1, 0)] # Right, Left, Down, Up

# Type alias for the map grid
type Grid = seq[seq[int]]
# Type alias for memoization table
type MemoTable = seq[seq[int64]] 

# Function to recursively calculate the number of paths to a height 9
# starting from cell (r, c). Uses memoization to avoid redundant calculations.
proc countPathsToNine(r, c: int, map: Grid, memo: var MemoTable): int64 =
  let rows = map.len
  let cols = if rows > 0: map[0].len else: 0

  # 1. Boundary Check
  if r < 0 or r >= rows or c < 0 or c >= cols:
    return 0

  # 2. Memoization Check
  if memo[r][c] != -1'i64:
    return memo[r][c]

  let currentHeight = map[r][c]

  # 3. Base Case: Reached a peak (height 9)
  if currentHeight == 9:
    return 1 # Found one valid path ending here

  # 4. Recursive Step: Explore neighbors with height + 1
  var pathsFromHere: int64 = 0
  let nextHeight = currentHeight + 1

  for (dr, dc) in Directions:
    let nr = r + dr
    let nc = c + dc
    
    # Check neighbor bounds and height condition *before* recursive call
    if nr >= 0 and nr < rows and nc >= 0 and nc < cols and map[nr][nc] == nextHeight:
       pathsFromHere += countPathsToNine(nr, nc, map, memo)

  # 5. Store result in memo table and return
  memo[r][c] = pathsFromHere
  return pathsFromHere

# Main logic encapsulated in a procedure
proc solve() =
  # Read map from input.txt
  var map: Grid
  try:
    let content = readFile("input.txt")
    for line in content.splitLines:
      if line.len > 0:
        map.add(line.map(proc(c: char): int = parseInt($c)))
  except IOError:
    echo "Error: Could not read input.txt"
    return
  except ValueError:
    echo "Error: Invalid character found in input.txt. Expected digits 0-9."
    return

  if map.len == 0 or map[0].len == 0:
    echo "Error: Input map is empty."
    return
    
  let rows = map.len
  let cols = map[0].len

  # Find all trailheads (height 0)
  var trailheads: seq[(int, int)]
  for r in 0..<rows:
    for c in 0..<cols:
      if map[r][c] == 0:
        trailheads.add((r, c))

  # Calculate the rating for each trailhead and sum them up
  var totalRatingSum: int64 = 0
  for (startR, startC) in trailheads:
    # Initialize memoization table for this trailhead (-1 indicates not computed)
    # Use int64 for rating as it can grow large
    var memo = newSeqWith(rows, newSeqWith(cols, -1'i64)) 
    
    # The rating is the number of paths starting from the trailhead (height 0)
    # and reaching any 9.
    let rating = countPathsToNine(startR, startC, map, memo)
    totalRatingSum += rating

  # Print the final result to standard output
  echo totalRatingSum

# Proper main entry point
when isMainModule:
  solve()

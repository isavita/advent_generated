
import std/[strutils, sequtils, algorithm, deques, os]

# --- Constants ---
const InputFile = "input.txt"

# --- Types ---
type
  Coord = tuple[r, c: int]
  HeightMap = seq[seq[int]]

# --- Helper Procedures ---

proc readHeightMap(filename: string): HeightMap =
  ## Reads the height map from the specified file.
  result = @[]
  if not fileExists(filename):
    quit("Error: Input file '" & filename & "' not found.", 1)
  
  for line in lines(filename):
    if line.len > 0:
      try:
        result.add(line.map(proc(c: char): int = parseInt($c)))
      except ValueError:
        quit("Error: Invalid character in input file. Expecting digits 0-9.", 1)
    # Allow empty lines if needed, but typically AoC inputs are dense
  
  # Validate grid consistency (all rows same length)
  if result.len > 1:
    let expectedLen = result[0].len
    for i in 1..<result.len:
      if result[i].len != expectedLen:
        quit("Error: Input grid rows have inconsistent lengths.", 1)

proc getNeighbors(r, c, numRows, numCols: int): seq[Coord] =
  ## Returns valid adjacent coordinates (up, down, left, right).
  result = @[]
  let dr = [-1, 1, 0, 0] # Delta rows
  let dc = [0, 0, -1, 1] # Delta columns

  for i in 0..3:
    let nr = r + dr[i]
    let nc = c + dc[i]
    if nr >= 0 and nr < numRows and nc >= 0 and nc < numCols:
      result.add((nr, nc))

# --- Part 1 Logic ---

proc solvePart1(heightmap: HeightMap): int =
  ## Calculates the sum of risk levels for all low points.
  if heightmap.len == 0 or heightmap[0].len == 0:
    return 0 # Handle empty grid

  let numRows = heightmap.len
  let numCols = heightmap[0].len
  var totalRiskSum = 0

  for r in 0..<numRows:
    for c in 0..<numCols:
      let currentHeight = heightmap[r][c]
      var isLowPoint = true
      for neighbor in getNeighbors(r, c, numRows, numCols):
        if heightmap[neighbor.r][neighbor.c] <= currentHeight:
          isLowPoint = false
          break # Not a low point

      if isLowPoint:
        totalRiskSum += currentHeight + 1

  return totalRiskSum

# --- Part 2 Logic ---

proc solvePart2(heightmap: HeightMap): int =
  ## Finds the three largest basins and returns the product of their sizes.
  if heightmap.len == 0 or heightmap[0].len == 0:
    return 0 # Handle empty grid

  let numRows = heightmap.len
  let numCols = heightmap[0].len
  var visited = newSeqWith(numRows, newSeq[bool](numCols)) # Initialize visited grid to false
  var basinSizes: seq[int] = @[]

  for r in 0..<numRows:
    for c in 0..<numCols:
      # If not a wall (9) and not yet visited, it's part of a new basin
      if heightmap[r][c] != 9 and not visited[r][c]:
        var currentBasinSize = 0
        var q = initDeque[Coord]() # Queue for BFS

        # Start BFS from this unvisited point
        q.addLast((r, c))
        visited[r][c] = true

        while q.len > 0:
          let currentCoord = q.popFirst()
          currentBasinSize += 1

          # Explore neighbors
          for neighbor in getNeighbors(currentCoord.r, currentCoord.c, numRows, numCols):
            if heightmap[neighbor.r][neighbor.c] != 9 and not visited[neighbor.r][neighbor.c]:
              visited[neighbor.r][neighbor.c] = true
              q.addLast(neighbor)
        
        # Finished exploring this basin
        basinSizes.add(currentBasinSize)

  # Find the product of the top 3 largest basin sizes
  if basinSizes.len < 3:
    echo "Warning: Found fewer than 3 basins." 
    # Calculate product of available sizes, or return 0 if none.
    if basinSizes.len == 0: return 0
    var product = 1
    for size in basinSizes: product *= size
    return product
  else:
    basinSizes.sort(SortOrder.Descending)
    return basinSizes[0] * basinSizes[1] * basinSizes[2]

# --- Main Entry Point ---

proc main() =
  ## Main procedure to run the solutions.
  let heightmap = readHeightMap(InputFile)
  
  if heightmap.len == 0:
    echo "Input file is empty or invalid."
    return

  # Part 1
  let riskSum = solvePart1(heightmap)
  echo "Part 1: ", riskSum

  # Part 2
  let basinProduct = solvePart2(heightmap)
  echo "Part 2: ", basinProduct

# --- Execute Main ---

when isMainModule:
  main()


import std/[strutils, sequtils, algorithm, sets, deques, math, os]

# --- Type Definitions ---
type
  Coord = tuple[r, c: int] # (row, column) coordinates

# --- Breadth-First Search (BFS) ---
# Finds the shortest distance between two points on the grid, avoiding walls ('#').
proc bfs(grid: seq[string], start: Coord, target: Coord): int =
  let R = grid.len
  if R == 0: return int.high # Handle empty grid case
  let C = grid[0].len
  if C == 0: return int.high # Handle empty row case

  var q = initDeque[tuple[pos: Coord, dist: int]]()
  var visited = initHashSet[Coord]()

  q.addLast((start, 0))
  visited.incl(start)

  while q.len > 0:
    let (curr, dist) = q.popFirst()

    if curr == target:
      return dist

    # Explore neighbors (Up, Down, Left, Right)
    const deltas = [( - 1, 0), (1, 0), (0, -1), (0, 1)] # dr, dc
    for (dr, dc) in deltas:
      let nextR = curr.r + dr
      let nextC = curr.c + dc
      let nextCoord = (r: nextR, c: nextC)

      # 1. Check bounds
      if nextR >= 0 and nextR < R and nextC >= 0 and nextC < C:
        # 2. Check wall
        if grid[nextR][nextC] != '#':
          # 3. Check visited
          if nextCoord notin visited:
            visited.incl(nextCoord)
            q.addLast((nextCoord, dist + 1))

  return int.high # Indicate unreachable

# --- Main Logic ---
proc solve(): int =
  let input = readFile("input.txt")
  let grid = input.strip.splitLines

  if grid.len == 0:
    echo "Error: Input file is empty or invalid."
    quit(1)

  var locations: seq[Coord] = @[] # Index = number, value = Coord
  var maxNum = -1

  # 1. Find coordinates of all numbered locations (0, 1, 2, ...)
  for r in 0..<grid.len:
    for c in 0..<grid[r].len:
      if grid[r][c].isDigit():
        let num = parseInt($grid[r][c])
        if num > maxNum:
          maxNum = num
          # Ensure sequence is large enough, potentially resizing with default coords
          if locations.len <= num:
             let oldLen = locations.len
             locations.setLen(num + 1)
             # Initialize new elements if needed (though default is usually fine)
             # for i in oldLen..locations.high: locations[i] = (r: -1, c: -1)
        locations[num] = (r: r, c: c)

  if maxNum == -1:
     echo "Error: No numbered locations found (including start '0')."
     quit(1)
  if locations[0] == default(Coord): # Check if '0' was found
      echo "Error: Start location '0' not found."
      quit(1)


  let numLocations = maxNum + 1

  # 2. Calculate all-pairs shortest paths using BFS
  # `distMatrix[i][j]` stores the shortest distance between location `i` and `j`.
  var distMatrix = newSeqWith(numLocations, newSeq[int](numLocations))
  for i in 0..<numLocations:
    # Skip locations that were not found on the map (holes in numbering)
    if locations[i] == default(Coord): continue
    for j in i..<numLocations:
      # Skip locations that were not found on the map
      if locations[j] == default(Coord): continue

      if i == j:
        distMatrix[i][j] = 0
      else:
        let d = bfs(grid, locations[i], locations[j])
        if d == int.high:
          echo "Error: Location ", j, " is unreachable from location ", i
          quit(1) # Problem assumes all locations are reachable
        distMatrix[i][j] = d
        distMatrix[j][i] = d # Distance is symmetric

  # 3. Solve the Traveling Salesperson-like problem
  # Find the shortest path starting at 0 and visiting all other numbers (1 to maxNum)
  var targets = newSeq[int]()
  for i in 1..maxNum:
      # Only include targets that actually exist on the map
      if locations[i] != default(Coord):
          targets.add(i)

  var minTotalDist = int.high

  if targets.len == 0: # Only location '0' exists or needs visiting
      minTotalDist = 0
  else:
      # Iterate through all permutations of the target locations
      while true: # Using do..while pattern with nextPermutation
          # Calculate distance for the current permutation: 0 -> p1 -> p2 -> ... -> pN
          var currentDist = distMatrix[0][targets[0]] # Start from 0 to the first target in permutation

          for k in 0 ..< targets.len - 1:
              currentDist += distMatrix[targets[k]][targets[k+1]]

          minTotalDist = min(minTotalDist, currentDist)

          if not nextPermutation(targets):
              break # No more permutations left

  return minTotalDist

# --- Entry Point ---
proc main() =
  # Check if input file exists
  if not fileExists("input.txt"):
    echo "Error: input.txt not found."
    quit(1)

  let result = solve()
  echo result

# Execute the main procedure
when isMainModule:
  main()

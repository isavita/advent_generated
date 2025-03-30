
import heapqueue, tables, strutils, os

type
  Coord = tuple[x, y: int]
  Grid = Table[Coord, char]
  DistTable = Table[Coord, int]
  PQItem = tuple[dist: int, coord: Coord] # dist first for default comparison

proc dijkstra(grid: Grid, endCoord: Coord): DistTable =
  var pq = initHeapQueue[PQItem]()
  pq.push((dist: 0, coord: endCoord))
  result = initTable[Coord, int]()
  result[endCoord] = 0

  # Precompute neighbor offsets
  let moves = [(0, 1), (0, -1), (1, 0), (-1, 0)]

  while pq.len > 0:
    let (d, curr) = pq.pop()

    # Optimization: If we've found a shorter path to `curr` already, skip processing
    # getOrDefault provides a large value if `curr` isn't in `result` yet
    if d > result.getOrDefault(curr, high(int)):
      continue

    for move in moves:
      let nextCoord = (x: curr.x + move[0], y: curr.y + move[1])

      # Check if neighbor is within the grid
      if grid.contains(nextCoord):
        # Check elevation rule (reversed: current must be at most 1 higher than next)
        if ord(grid[curr]) - ord(grid[nextCoord]) <= 1:
          let nextDist = d + 1
          # If this path is shorter than any previously found path to nextCoord
          if nextDist < result.getOrDefault(nextCoord, high(int)):
            result[nextCoord] = nextDist
            pq.push((dist: nextDist, coord: nextCoord))

proc main() =
  var grid: Grid = initTable[Coord, char]()
  var startCoord: Coord
  var endCoord: Coord
  var y = 0

  # Read from input.txt
  for line in lines "input.txt":
    for x, b in line:
      let p: Coord = (x, y)
      grid[p] = b
      if b == 'S':
        startCoord = p
      elif b == 'E':
        endCoord = p
    inc y # Increment y after processing each line

  # Standardize start/end elevations
  grid[startCoord] = 'a'
  grid[endCoord] = 'z'

  # Run Dijkstra starting from the end point
  let dists = dijkstra(grid, endCoord)

  # Print the distance to the original start point
  if dists.contains(startCoord):
    echo dists[startCoord]
  else:
    # Should not happen with valid inputs based on problem description
    echo "Start location not reachable from End location"

when isMainModule:
  main()


import heapqueue, tables, strutils

type Coord = tuple[x, y: int]

proc djikstra(grid: seq[seq[char]], width, height: int, endCoord: Coord): Table[Coord, int] =
  var pq = initHeapQueue[tuple[dist: int, coord: Coord]]()
  result = initTable[Coord, int]() # Use implicit result variable

  pq.push((0, endCoord))
  result[endCoord] = 0

  while pq.len > 0:
    let (currDist, curr) = pq.pop()

    # Optimization: Skip if we found a shorter path already
    if currDist > result.getOrDefault(curr, high(int)): 
        continue 

    let currElevation = grid[curr.y][curr.x]

    for delta in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
      let nextP: Coord = (curr.x + delta[0], curr.y + delta[1])

      # Bounds check
      if nextP.x < 0 or nextP.x >= width or nextP.y < 0 or nextP.y >= height:
        continue

      let nextElevation = grid[nextP.y][nextP.x]

      # Movement constraint (backwards): Can move to 'nextP' if its elevation is 
      # not more than 1 lower than 'curr' elevation.
      # ord(currElevation) - ord(nextElevation) <= 1
      if ord(currElevation) - ord(nextElevation) > 1: 
        continue

      let nextDist = currDist + 1
      # If this path is shorter than any previously found path to nextP
      if nextDist < result.getOrDefault(nextP, high(int)):
         result[nextP] = nextDist
         pq.push((nextDist, nextP))

proc main() =
  var grid: seq[seq[char]]
  var startCoord: Coord = (-1, -1) # Initialize to invalid
  var endCoord: Coord = (-1, -1)   # Initialize to invalid
  var aCoords: seq[Coord]
  var width = 0
  var height = 0

  for line in "input.txt".lines:
    let stripped = line.strip
    if stripped.len == 0: continue # Skip empty lines
    
    # Determine width from the first non-empty line
    if width == 0: width = stripped.len 
    
    var row = newSeq[char](width) # Pre-allocate row for efficiency
    var x = 0
    for b in stripped:
      let p: Coord = (x, height) # Use height as current y before incrementing
      var elevation = b
      case b:
      of 'S': 
        startCoord = p
        elevation = 'a' # Change elevation immediately
      of 'E': 
        endCoord = p
        elevation = 'z' # Change elevation immediately
      else: discard
      
      row[x] = elevation
      if elevation == 'a':
          aCoords.add(p) # Collect all 'a' coordinates (including original 'S')
      
      inc x
    grid.add(row)
    inc height # Increment height after processing the row

  let dists = djikstra(grid, width, height, endCoord)

  var minDist = high(int)
  for a in aCoords:
    # Check if 'a' position is reachable from the end
    if dists.contains(a):
        minDist = min(minDist, dists[a])

  echo minDist

main()

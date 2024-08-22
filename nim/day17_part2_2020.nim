import strutils, sequtils, tables, sets

type
  Coord = tuple[x, y, z, w: int]
  Grid = Table[Coord, bool]

proc parseInput(filename: string): Grid =
  let input = readFile(filename).splitLines()
  var grid: Grid
  for y, line in input:
    for x, char in line:
      if char == '#':
        grid[(x, y, 0, 0)] = true
  grid

proc getNeighbors(coord: Coord): seq[Coord] =
  var neighbors: seq[Coord] = @[]
  for dx in -1..1:
    for dy in -1..1:
      for dz in -1..1:
        for dw in -1..1:
          if dx == 0 and dy == 0 and dz == 0 and dw == 0:
            continue
          neighbors.add((coord.x + dx, coord.y + dy, coord.z + dz, coord.w + dw))
  neighbors

proc countActiveNeighbors(grid: Grid, coord: Coord): int =
  var count = 0
  for neighbor in getNeighbors(coord):
    if grid.hasKey(neighbor) and grid[neighbor]:
      count += 1
  count

proc simulateCycle(grid: Grid): Grid =
  var newGrid: Grid
  var toCheck: seq[Coord] = @[]
  for coord in grid.keys:
    toCheck.add(coord)
    toCheck.add(getNeighbors(coord))
  toCheck = toSeq(toSet(toCheck))  # Remove duplicates
  for coord in toCheck:
    let activeNeighbors = countActiveNeighbors(grid, coord)
    if grid.hasKey(coord) and grid[coord]:
      if activeNeighbors == 2 or activeNeighbors == 3:
        newGrid[coord] = true
    else:
      if activeNeighbors == 3:
        newGrid[coord] = true
  newGrid

proc countActiveCubes(grid: Grid): int =
  var count = 0
  for coord in grid.keys:
    if grid[coord]:
      count += 1
  count

proc main() =
  var grid = parseInput("input.txt")
  for _ in 1..6:
    grid = simulateCycle(grid)
  echo "Active cubes after 6 cycles: ", countActiveCubes(grid)

main()
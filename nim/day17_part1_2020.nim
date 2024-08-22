import sequtils, strutils, tables

type
  Coord = tuple[x, y, z: int]
  CubeState = enum Active, Inactive

proc parseInput(filename: string): Table[Coord, CubeState] =
  var cubes = initTable[Coord, CubeState]()
  let lines = readFile(filename).splitLines()
  for y, line in lines.pairs:
    for x, char in line.pairs:
      if char == '#':
        cubes[(x.int, y.int, 0)] = Active
  cubes

proc countActiveNeighbors(cubes: Table[Coord, CubeState], coord: Coord): int =
  var count = 0
  for dx in -1..1:
    for dy in -1..1:
      for dz in -1..1:
        if dx == 0 and dy == 0 and dz == 0:
          continue
        let neighbor = (coord.x + dx, coord.y + dy, coord.z + dz)
        if cubes.getOrDefault(neighbor, Inactive) == Active:
          count.inc
  count

proc simulateCycle(cubes: Table[Coord, CubeState]): Table[Coord, CubeState] =
  var newCubes = initTable[Coord, CubeState]()
  var minX, minY, minZ, maxX, maxY, maxZ: int
  for coord in cubes.keys:
    minX = min(minX, coord.x)
    minY = min(minY, coord.y)
    minZ = min(minZ, coord.z)
    maxX = max(maxX, coord.x)
    maxY = max(maxY, coord.y)
    maxZ = max(maxZ, coord.z)
  for x in minX-1..maxX+1:
    for y in minY-1..maxY+1:
      for z in minZ-1..maxZ+1:
        let coord = (x, y, z)
        let activeNeighbors = countActiveNeighbors(cubes, coord)
        if cubes.getOrDefault(coord, Inactive) == Active:
          if activeNeighbors == 2 or activeNeighbors == 3:
            newCubes[coord] = Active
        else:
          if activeNeighbors == 3:
            newCubes[coord] = Active
  newCubes

proc countActiveCubes(cubes: Table[Coord, CubeState]): int =
  var count = 0
  for state in cubes.values:
    if state == Active:
      count.inc
  count

let initialCubes = parseInput("input.txt")
var cubes = initialCubes
for _ in 1..6:
  cubes = simulateCycle(cubes)

echo "Active cubes after 6 cycles: ", countActiveCubes(cubes)
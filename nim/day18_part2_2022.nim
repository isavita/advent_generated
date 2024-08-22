import strutils, sequtils, sets, tables

type Point = tuple[x, y, z: int]

proc parseInput(filename: string): seq[Point] =
  result = @[]
  let file = readFile(filename)
  for line in file.splitLines:
    let coords = line.split(',')
    result.add((parseInt(coords[0]), parseInt(coords[1]), parseInt(coords[2])))

proc getNeighbors(p: Point): seq[Point] =
  result = @[
    (p.x + 1, p.y, p.z), (p.x - 1, p.y, p.z),
    (p.x, p.y + 1, p.z), (p.x, p.y - 1, p.z),
    (p.x, p.y, p.z + 1), (p.x, p.y, p.z - 1)
  ]

proc countExposedSides(cubes: seq[Point]): int =
  let cubeSet = cubes.toHashSet
  var minX, minY, minZ, maxX, maxY, maxZ: int
  for cube in cubes:
    if cube.x < minX: minX = cube.x
    if cube.y < minY: minY = cube.y
    if cube.z < minZ: minZ = cube.z
    if cube.x > maxX: maxX = cube.x
    if cube.y > maxY: maxY = cube.y
    if cube.z > maxZ: maxZ = cube.z

  let start = (minX - 1, minY - 1, minZ - 1)
  var visited = initHashSet[Point]()
  var queue = @[start]
  visited.incl(start)

  while queue.len > 0:
    let current = queue.pop()
    for neighbor in getNeighbors(current):
      if neighbor.x < minX - 1 or neighbor.x > maxX + 1 or
         neighbor.y < minY - 1 or neighbor.y > maxY + 1 or
         neighbor.z < minZ - 1 or neighbor.z > maxZ + 1:
        continue
      if neighbor notin visited:
        if neighbor notin cubeSet:
          queue.add(neighbor)
          visited.incl(neighbor)
        else:
          result += 1

when isMainModule:
  let cubes = parseInput("input.txt")
  let exposedSides = countExposedSides(cubes)
  echo exposedSides

import strutils, sequtils, sets

type
  Cube = tuple[x, y, z: int]

proc readCubes(filename: string): seq[Cube] =
  result = @[]
  for line in readFile(filename).splitLines():
    let parts = line.split(',')
    result.add((parts[0].parseInt, parts[1].parseInt, parts[2].parseInt))

proc countSurfaceArea(cubes: seq[Cube]): int =
  let cubeSet = cubes.toSet()
  var surfaceArea = 0

  for cube in cubes:
    for dx, dy, dz in items([(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]):
      let neighbor = (cube.x + dx, cube.y + dy, cube.z + dz)
      if neighbor notin cubeSet:
        inc(surfaceArea)

  return surfaceArea

proc main() =
  let cubes = readCubes("input.txt")
  let surfaceArea = countSurfaceArea(cubes)
  echo surfaceArea

main()

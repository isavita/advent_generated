import strutils, sequtils, math

type Cuboid = object
  x1, x2, y1, y2, z1, z2: int
  on: bool

proc parseInput(filename: string): seq[Cuboid] =
  var cuboids: seq[Cuboid] = @[]
  for line in lines(filename):
    let parts = line.split(' ')
    let on = parts[0] == "on"
    let coords = parts[1].split(',')
    let xRange = coords[0][2..^1].split("..")
    let yRange = coords[1][2..^1].split("..")
    let zRange = coords[2][2..^1].split("..")
    let x1 = parseInt(xRange[0])
    let x2 = parseInt(xRange[1])
    let y1 = parseInt(yRange[0])
    let y2 = parseInt(yRange[1])
    let z1 = parseInt(zRange[0])
    let z2 = parseInt(zRange[1])
    cuboids.add(Cuboid(x1: x1, x2: x2, y1: y1, y2: y2, z1: z1, z2: z2, on: on))
  return cuboids

proc intersect(c1, c2: Cuboid): Cuboid =
  let x1 = max(c1.x1, c2.x1)
  let x2 = min(c1.x2, c2.x2)
  let y1 = max(c1.y1, c2.y1)
  let y2 = min(c1.y2, c2.y2)
  let z1 = max(c1.z1, c2.z1)
  let z2 = min(c1.z2, c2.z2)
  if x1 <= x2 and y1 <= y2 and z1 <= z2:
    return Cuboid(x1: x1, x2: x2, y1: y1, y2: y2, z1: z1, z2: z2, on: not c1.on)
  else:
    return Cuboid(x1: 0, x2: -1, y1: 0, y2: -1, z1: 0, z2: -1, on: false)

proc volume(c: Cuboid): int64 =
  result = int64(c.x2 - c.x1 + 1) * int64(c.y2 - c.y1 + 1) * int64(c.z2 - c.z1 + 1)
  if not c.on:
    result = -result

proc solve(filename: string): int64 =
  let cuboids = parseInput(filename)
  var activeCuboids: seq[Cuboid] = @[]
  for cuboid in cuboids:
    var newCuboids: seq[Cuboid] = @[]
    for active in activeCuboids:
      let intersection = intersect(active, cuboid)
      if intersection.x1 <= intersection.x2:
        newCuboids.add(intersection)
    if cuboid.on:
      newCuboids.add(cuboid)
    activeCuboids.add(newCuboids)
  result = activeCuboids.mapIt(volume(it)).foldl(a + b, 0'i64)

echo solve("input.txt")
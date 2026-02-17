
import std/[strutils, strscans, heapqueue, algorithm, math]

type
  Nanobot = object
    x, y, z, r: int

  Cube = object
    count: int
    dist: int64
    size: int
    x, y, z: int

proc `<`(a, b: Cube): bool =
  if a.count != b.count: return a.count > b.count
  if a.dist != b.dist: return a.dist < b.dist
  return a.size > b.size

proc manhattan(x1, y1, z1, x2, y2, z2: int): int64 =
  abs(x1.int64 - x2.int64) + abs(y1.int64 - y2.int64) + abs(z1.int64 - z2.int64)

proc distToOrigin(x, y, z, size: int): int64 =
  var dx, dy, dz: int64
  if x > 0: dx = x.int64
  elif x + size - 1 < 0: dx = -(x.int64 + size - 1)
  if y > 0: dy = y.int64
  elif y + size - 1 < 0: dy = -(y.int64 + size - 1)
  if z > 0: dz = z.int64
  elif z + size - 1 < 0: dz = -(z.int64 + size - 1)
  dx + dy + dz

proc botInCube(bot: Nanobot, x, y, z, size: int): bool =
  var d: int64 = 0
  if bot.x < x: d += (x - bot.x)
  elif bot.x > x + size - 1: d += (bot.x - (x + size - 1))
  if bot.y < y: d += (y - bot.y)
  elif bot.y > y + size - 1: d += (bot.y - (y + size - 1))
  if bot.z < z: d += (z - bot.z)
  elif bot.z > z + size - 1: d += (bot.z - (z + size - 1))
  d <= bot.r

proc solve() =
  var nanobots: seq[Nanobot]
  for line in lines("input.txt"):
    var nb: Nanobot
    if scanf(line, "pos=<$i,$i,$i>, r=$i", nb.x, nb.y, nb.z, nb.r):
      nanobots.add(nb)

  var strongest = nanobots[0]
  for i in 1..<nanobots.len:
    if nanobots[i].r > strongest.r: strongest = nanobots[i]

  var p1 = 0
  for b in nanobots:
    if manhattan(strongest.x, strongest.y, strongest.z, b.x, b.y, b.z) <= strongest.r.int64:
      inc p1
  echo p1

  var minX, maxX, minY, maxY, minZ, maxZ = nanobots[0].x
  for b in nanobots:
    minX = min(minX, b.x); maxX = max(maxX, b.x)
    minY = min(minY, b.y); maxY = max(maxY, b.y)
    minZ = min(minZ, b.z); maxZ = max(maxZ, b.z)

  var size = 1
  while size < max([maxX - minX + 1, maxY - minY + 1, maxZ - minZ + 1]): size *= 2

  var pq = HeapQueue[Cube]()
  var initialCount = 0
  for b in nanobots:
    if b.botInCube(minX, minY, minZ, size): inc initialCount
  
  pq.push(Cube(count: initialCount, dist: distToOrigin(minX, minY, minZ, size), size: size, x: minX, y: minY, z: minZ))

  while pq.len > 0:
    let curr = pq.pop()
    if curr.size == 1:
      echo curr.dist
      break

    let half = curr.size div 2
    for dx in 0..1:
      for dy in 0..1:
        for dz in 0..1:
          let nx = curr.x + dx * half
          let ny = curr.y + dy * half
          let nz = curr.z + dz * half
          var count = 0
          for b in nanobots:
            if b.botInCube(nx, ny, nz, half): inc count
          pq.push(Cube(count: count, dist: distToOrigin(nx, ny, nz, half), size: half, x: nx, y: ny, z: nz))

solve()


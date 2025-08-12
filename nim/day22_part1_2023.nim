
import std/[strutils, sequtils, algorithm, os]

type
  Brick = object
    x1, y1, z1, x2, y2, z2: int

proc intersects(a, b: Brick): bool =
  max(a.x1, b.x1) <= min(a.x2, b.x2) and
  max(a.y1, b.y1) <= min(a.y2, b.y2)

proc readBricks(fileName: string): seq[Brick] =
  var bricks: seq[Brick] = @[]
  for line in lines(fileName):
    let parts = line.split('~')
    let s = parts[0].split(',')
    let e = parts[1].split(',')
    bricks.add Brick(
      x1: s[0].parseInt,
      y1: s[1].parseInt,
      z1: s[2].parseInt,
      x2: e[0].parseInt,
      y2: e[1].parseInt,
      z2: e[2].parseInt
    )
  bricks

proc settleBricks(bricks: var seq[Brick]) =
  bricks.sort(proc(a, b: Brick): int = a.z1 - b.z1)
  for i in 0 ..< bricks.len:
    var maxZ = 1
    for j in 0 ..< i:
      if bricks[i].intersects(bricks[j]):
        maxZ = max(maxZ, bricks[j].z2 + 1)
    bricks[i].z2 -= (bricks[i].z1 - maxZ)
    bricks[i].z1 = maxZ
  bricks.sort(proc(a, b: Brick): int = a.z1 - b.z1)

proc isSafeToDisintegrate(bricks: seq[Brick], idx: int): bool =
  let dis = bricks[idx]
  for i in idx+1 ..< bricks.len:
    let above = bricks[i]
    if above.z1 <= dis.z2: continue
    var supported = false
    for j in 0 ..< bricks.len:
      if i == j or j == idx: continue
      let other = bricks[j]
      if above.z1 == other.z2 + 1 and above.intersects(other):
        supported = true
        break
    if not supported: return false
  true

proc main =
  var bricks = readBricks("input.txt")
  settleBricks(bricks)
  var safe = 0
  for i in 0 ..< bricks.len:
    if isSafeToDisintegrate(bricks, i):
      inc safe
  echo safe

main()

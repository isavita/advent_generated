
import std/[strutils, sequtils, tables, sets, os]

type
  Point = tuple[x, y, z: int]

const rotations: array[24, array[3, array[3, int]]] = [
  [[ 1, 0, 0], [ 0, 1, 0], [ 0, 0, 1]], [[ 1, 0, 0], [ 0, 0,-1], [ 0, 1, 0]],
  [[ 1, 0, 0], [ 0,-1, 0], [ 0, 0,-1]], [[ 1, 0, 0], [ 0, 0, 1], [ 0,-1, 0]],
  [[ 0,-1, 0], [ 1, 0, 0], [ 0, 0, 1]], [[ 0, 0, 1], [ 1, 0, 0], [ 0, 1, 0]],
  [[ 0, 1, 0], [ 1, 0, 0], [ 0, 0,-1]], [[ 0, 0,-1], [ 1, 0, 0], [ 0,-1, 0]],
  [[-1, 0, 0], [ 0,-1, 0], [ 0, 0, 1]], [[-1, 0, 0], [ 0, 0,-1], [ 0,-1, 0]],
  [[-1, 0, 0], [ 0, 1, 0], [ 0, 0,-1]], [[-1, 0, 0], [ 0, 0, 1], [ 0, 1, 0]],
  [[ 0, 1, 0], [-1, 0, 0], [ 0, 0, 1]], [[ 0, 0, 1], [-1, 0, 0], [ 0,-1, 0]],
  [[ 0,-1, 0], [-1, 0, 0], [ 0, 0,-1]], [[ 0, 0,-1], [-1, 0, 0], [ 0, 1, 0]],
  [[ 0, 0,-1], [ 0, 1, 0], [ 1, 0, 0]], [[ 0, 1, 0], [ 0, 0, 1], [ 1, 0, 0]],
  [[ 0, 0, 1], [ 0,-1, 0], [ 1, 0, 0]], [[ 0,-1, 0], [ 0, 0,-1], [ 1, 0, 0]],
  [[ 0, 0,-1], [ 0,-1, 0], [-1, 0, 0]], [[ 0,-1, 0], [ 0, 0, 1], [-1, 0, 0]],
  [[ 0, 0, 1], [ 0, 1, 0], [-1, 0, 0]], [[ 0, 1, 0], [ 0, 0,-1], [-1, 0, 0]]
]

proc rotate(p: Point; idx: int): Point =
  let r = rotations[idx]
  (p.x * r[0][0] + p.y * r[0][1] + p.z * r[0][2],
   p.x * r[1][0] + p.y * r[1][1] + p.z * r[1][2],
   p.x * r[2][0] + p.y * r[2][1] + p.z * r[2][2])

proc add(a, b: Point): Point = (a.x + b.x, a.y + b.y, a.z + b.z)
proc sub(a, b: Point): Point = (a.x - b.x, a.y - b.y, a.z - b.z)
proc manhattan(a, b: Point): int = abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)

proc readInput(fname: string): seq[seq[Point]] =
  var scanners: seq[seq[Point]] = @[]
  var cur: seq[Point] = @[]
  for line in lines(fname):
    if line.startsWith("---"):
      if cur.len > 0:
        scanners.add cur
        cur = @[]
    elif line.strip.len > 0:
      let parts = line.split(',')
      if parts.len == 3:
        cur.add (parseInt(parts[0]), parseInt(parts[1]), parseInt(parts[2]))
  if cur.len > 0:
    scanners.add cur
  scanners

proc solve(scanners: seq[seq[Point]]): int =
  var global = initHashSet[Point]()
  for p in scanners[0]: global.incl p
  var scannerPos = @[ (0,0,0) ]
  var aligned = newSeq[bool](scanners.len)
  aligned[0] = true
  var alignedCount = 1

  while alignedCount < scanners.len:
    var found = false
    for i in 1..<scanners.len:
      if aligned[i]: continue
      for rot in 0..<24:
        var rotated: seq[Point] = @[]
        for p in scanners[i]: rotated.add rotate(p, rot)

        var deltaCnt = initTable[Point, int]()
        for rp in rotated:
          for gp in global:
            let d = sub(gp, rp)
            inc(deltaCnt.mgetOrPut(d, 0))
        var bestDelta: Point = (0,0,0)
        var maxCnt = 0
        for d, c in deltaCnt:
          if c > maxCnt:
            maxCnt = c
            bestDelta = d
        if maxCnt >= 12:
          scannerPos.add bestDelta
          aligned[i] = true
          inc alignedCount
          for rp in rotated:
            global.incl add(rp, bestDelta)
          found = true
          break
        # end rot
      if found: break
    if not found:
      return -1

  var maxDist = 0
  for i in 0..<scannerPos.len:
    for j in i+1..<scannerPos.len:
      let d = manhattan(scannerPos[i], scannerPos[j])
      if d > maxDist: maxDist = d
  maxDist

when isMainModule:
  let scanners = readInput("input.txt")
  if scanners.len == 0:
    echo 0
    quit 0
  let res = solve(scanners)
  if res >= 0:
    echo res
    quit 0
  else:
    stderr.writeLine "Solver failed"
    quit 1

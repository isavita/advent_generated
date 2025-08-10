import os, strutils, sequtils, algorithm, heapqueue

const
  GEOLOGIC_Y = 16807
  GEOLOGIC_X = 48271
  CAVE_MODULO = 20183
  PADDING = 100
  INF = 1_000_000_000

type
  State = tuple[time:int, x:int, y:int, tool:int]

proc cmp(a, b: State): bool =
  a.time < b.time

var
  depth, targetX, targetY: int
  lines = readFile("input.txt").splitLines()
  depthLine = lines[0]
  targetLine = lines[1]
  depthVal = parseInt(depthLine.split(":")[1].strip())
  targetParts = targetLine.split(":")[1].strip().split(",")
  targetXVal = parseInt(targetParts[0])
  targetYVal = parseInt(targetParts[1])

depth = depthVal
targetX = targetXVal
targetY = targetYVal

let boundX = targetX + PADDING
let boundY = targetY + PADDING

var erosion = newSeq[seq[int]](boundY)
for y in 0..<boundY:
  erosion[y] = newSeq[int](boundX)
  for x in 0..<boundX:
    erosion[y][x] = -1

proc getErosion(x, y: int): int =
  if erosion[y][x] != -1:
    return erosion[y][x]
  var geoIndex: int
  if x == 0 and y == 0:
    geoIndex = 0
  elif x == targetX and y == targetY:
    geoIndex = 0
  elif y == 0:
    geoIndex = x * GEOLOGIC_Y
  elif x == 0:
    geoIndex = y * GEOLOGIC_X
  else:
    geoIndex = getErosion(x-1, y) * getErosion(x, y-1)
  erosion[y][x] = (geoIndex + depth) mod CAVE_MODULO
  return erosion[y][x]

proc getType(x, y: int): int =
  getErosion(x, y) mod 3

proc allowedMask(t: int): int =
  case t
  of 0: 2 or 4          # rocky: torch, gear
  of 1: 4 or 1          # wet: gear, none
  else: 2 or 1           # narrow: torch, none

var dist = newSeq[seq[seq[int]]](boundY)
for y in 0..<boundY:
  dist[y] = newSeq[seq[int]](boundX)
  for x in 0..<boundX:
    dist[y][x] = newSeq[int](3)
    for t in 0..2:
      dist[y][x][t] = INF

var pq = initHeapQueue[State]()
dist[0][0][1] = 0
pq.push((time:0, x:0, y:0, tool:1))

let dirs = [(1,0),(-1,0),(0,1),(0,-1)]

var answer = -1
while pq.len > 0:
  let cur = pq.pop()
  if cur.time > dist[cur.y][cur.x][cur.tool]:
    continue
  if cur.x == targetX and cur.y == targetY and cur.tool == 1:
    answer = cur.time
    break
  let curType = getType(cur.x, cur.y)
  let mask = allowedMask(curType)
  for nt in 0..2:
    if nt != cur.tool and ((1 shl nt) and mask) != 0:
      let ntTime = cur.time + 7
      if ntTime < dist[cur.y][cur.x][nt]:
        dist[cur.y][cur.x][nt] = ntTime
        pq.push((time:ntTime, x:cur.x, y:cur.y, tool:nt))
  for d in dirs:
    let nx = cur.x + d[0]
    let ny = cur.y + d[1]
    if nx < 0 or ny < 0 or nx >= boundX or ny >= boundY:
      continue
    let nType = getType(nx, ny)
    let nMask = allowedMask(nType)
    if ((1 shl cur.tool) and nMask) != 0:
      let nTime = cur.time + 1
      if nTime < dist[ny][nx][cur.tool]:
        dist[ny][nx][cur.tool] = nTime
        pq.push((time:nTime, x:nx, y:ny, tool:cur.tool))

echo answer


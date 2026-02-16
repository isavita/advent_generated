
import std/[strutils, sequtils, algorithm, times]

type
  Point = tuple[x, y, z: int]
  Edge = tuple[u, v: int; d: int64]

proc readPoints(): seq[Point] =
  for line in "input.txt".lines:
    let p = line.split(',').mapIt(it.strip.parseInt)
    if p.len == 3:
      result.add (p[0], p[1], p[2])

proc find(parent: var seq[int]; x: int): int =
  var x = x
  while parent[x] != x:
    parent[x] = parent[parent[x]]
    x = parent[x]
  x

proc union(parent, size: var seq[int]; a, b: int) =
  let ra = parent.find(a)
  let rb = parent.find(b)
  if ra == rb: return
  if size[ra] < size[rb]:
    parent[ra] = rb
    size[rb] += size[ra]
  else:
    parent[rb] = ra
    size[ra] += size[rb]

proc cmp(a, b: Edge): int = cmp(a.d, b.d)

let pts = readPoints()
let n = pts.len
if n < 2: quit "0"

var edges = newSeq[Edge](n * (n - 1) div 2)
var idx = 0
for i in 0 ..< n:
  for j in i + 1 ..< n:
    let dx = (pts[i].x - pts[j].x).int64
    let dy = (pts[i].y - pts[j].y).int64
    let dz = (pts[i].z - pts[j].z).int64
    edges[idx] = (i, j, dx * dx + dy * dy + dz * dz)
    inc idx

edges.sort(cmp)

var parent = newSeq[int](n)
var size = newSeq[int](n)
for i in 0 ..< n:
  parent[i] = i
  size[i] = 1

let limit = min(edges.len, 1000)
for i in 0 ..< limit:
  parent.union(size, edges[i].u, edges[i].v)

var top = [0, 0, 0]
for i in 0 ..< n:
  if parent[i] == i:
    let s = size[i]
    if s > top[0]:
      top[2] = top[1]
      top[1] = top[0]
      top[0] = s
    elif s > top[1]:
      top[2] = top[1]
      top[1] = s
    elif s > top[2]:
      top[2] = s

var prod = 1'u64
for i in 0 .. 2:
  if top[i] > 0: prod *= top[i].uint64
echo prod


import std/[strutils, sequtils, algorithm]

type
  Point = tuple[x, y, z: int]
  Edge = tuple[u, v: int; d: int64]

proc distSq(a, b: Point): int64 =
  let dx = a.x - b.x
  let dy = a.y - b.y
  let dz = a.z - b.z
  result = dx*dx + dy*dy + dz*dz

proc find(parent: var seq[int], x: int): int =
  result = x
  while parent[result] != result:
    parent[result] = parent[parent[result]]
    result = parent[result]

proc unite(parent, rank: var seq[int], a, b: int) =
  var a = find(parent, a)
  var b = find(parent, b)
  if a == b: return
  if rank[a] < rank[b]:
    parent[a] = b
  elif rank[a] > rank[b]:
    parent[b] = a
  else:
    parent[b] = a
    inc rank[a]

var pts: seq[Point]
for line in lines("input.txt"):
  let parts = line.split(',')
  if parts.len == 3:
    pts.add((parseInt(parts[0]), parseInt(parts[1]), parseInt(parts[2])))

if pts.len < 2: quit(0)

var edges: seq[Edge]
for i in 0..<pts.len:
  for j in i+1..<pts.len:
    edges.add((i, j, distSq(pts[i], pts[j])))

edges.sort(proc(a, b: Edge): int = cmp(a.d, b.d))

var parent = newSeq[int](pts.len)
var rank = newSeq[int](pts.len)
for i in 0..<pts.len:
  parent[i] = i

var comps = pts.len
for e in edges:
  let (u, v, _) = e
  let ru = find(parent, u)
  let rv = find(parent, v)
  if ru != rv:
    unite(parent, rank, ru, rv)
    dec comps
    if comps == 1:
      let (x1, y1, z1) = pts[u]
      let (x2, y2, z2) = pts[v]
      echo "Connected ", x1, ",", y1, ",", z1, " and ", x2, ",", y2, ",", z2
      echo "Product of X coordinates: ", x1 * x2
      break

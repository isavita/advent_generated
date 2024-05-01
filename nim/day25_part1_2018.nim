import strutils, sequtils, math

type Point = object
  x, y, z, t: int

proc abs(x: int): int =
  if x < 0: -x else: x

proc manhattanDistance(a, b: Point): int =
  abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t)

type UnionFind = object
  parent: seq[int]

proc newUnionFind(size: int): UnionFind =
  result.parent = newSeq[int](size)
  for i in 0..<size:
    result.parent[i] = i

proc find(uf: var UnionFind, x: int): int =
  if uf.parent[x] != x:
    uf.parent[x] = uf.find(uf.parent[x])
  uf.parent[x]

proc union(uf: var UnionFind, x, y: int) =
  let rootX = uf.find(x)
  let rootY = uf.find(y)
  if rootX != rootY:
    uf.parent[rootX] = rootY

when isMainModule:
  let file = open("input.txt")
  var points: seq[Point]
  for line in file.lines:
    let coords = line.split(',')
    points.add(Point(x: coords[0].parseInt, y: coords[1].parseInt, z: coords[2].parseInt, t: coords[3].parseInt))
  file.close()  # Move the close statement here

  var uf = newUnionFind(points.len)
  for i in 0..<points.len:
    for j in 0..<points.len:
      if manhattanDistance(points[i], points[j]) <= 3:
        uf.union(i, j)

  var constellationCount = 0
  for i, parent in uf.parent:
    if i == parent:
      inc constellationCount
  echo constellationCount
fs = require 'fs'

class Point
  constructor: (@x, @y, @z, @t) ->

abs = (x) -> if x < 0 then -x else x

manhattanDistance = (a, b) ->
  abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t)

class UnionFind
  constructor: (size) ->
    @parent = [0...size]

  find: (x) ->
    if @parent[x] != x
      @parent[x] = @find(@parent[x])
    @parent[x]

  union: (x, y) ->
    rootX = @find(x)
    rootY = @find(y)
    @parent[rootX] = rootY if rootX != rootY

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  points = []
  lines = data.trim().split '\n'
  for line in lines
    [x, y, z, t] = line.split(',').map (num) -> parseInt num, 10
    points.push new Point x, y, z, t

  uf = new UnionFind points.length
  for i in [0...points.length]
    for j in [0...points.length]
      uf.union(i, j) if manhattanDistance(points[i], points[j]) <= 3

  constellationCount = 0
  for i in [0...uf.parent.length]
    constellationCount++ if i == uf.parent[i]

  console.log constellationCount
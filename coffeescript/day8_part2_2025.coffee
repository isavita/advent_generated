
fs = require 'fs'

lines = fs.readFileSync('input.txt','utf8').split(/\r?\n/)

pts = []
for line in lines
  continue if /^\s*$/.test line
  m = line.match /(-?\d+),(-?\d+),(-?\d+)/
  if m
    pts.push {x:+m[1], y:+m[2], z:+m[3]}

n = pts.length
process.exit 0 if n < 2

edges = []
for i in [0...n]
  pi = pts[i]
  for j in [i+1...n]
    pj = pts[j]
    dx = pi.x - pj.x
    dy = pi.y - pj.y
    dz = pi.z - pj.z
    edges.push {u:i, v:j, d:dx*dx + dy*dy + dz*dz}

edges.sort (a,b) -> a.d - b.d

parent = (i for i in [0...n])
rank   = (0 for i in [0...n])

find = (x) ->
  while parent[x] != x
    parent[x] = parent[parent[x]]
    x = parent[x]
  x

unite = (a,b) ->
  a = find a
  b = find b
  return if a == b
  if rank[a] < rank[b]
    parent[a] = b
  else if rank[a] > rank[b]
    parent[b] = a
  else
    parent[b] = a
    rank[a]++

comps = n
for e in edges
  ru = find e.u
  rv = find e.v
  if ru != rv
    unite ru, rv
    comps--
    if comps == 1
      p1 = pts[e.u]
      p2 = pts[e.v]
      console.log "Connected #{p1.x},#{p1.y},#{p1.z} and #{p2.x},#{p2.y},#{p2.z}"
      console.log "Product of X coordinates: #{p1.x * p2.x}"
      break

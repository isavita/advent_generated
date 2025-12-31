
fs = require 'fs'

pts = []
for line in fs.readFileSync('input.txt','utf8').trim().split /\r?\n/
  m = line.match /(-?\d+)\s*,\s*(-?\d+)\s*,\s*(-?\d+)/
  pts.push [parseInt(m[1]),parseInt(m[2]),parseInt(m[3])] if m

n = pts.length
if n < 2
  console.log "Not enough points to form circuits."
  process.exit 0

edges = []
for i in [0...n]
  for j in [i+1...n]
    dx = pts[i][0]-pts[j][0]
    dy = pts[i][1]-pts[j][1]
    dz = pts[i][2]-pts[j][2]
    edges.push {u:i,v:j,d:dx*dx+dy*dy+dz*dz}
edges.sort (a,b)->a.d-b.d

class UF
  constructor:(n)->
    @p = (i for i in [0...n])
    @sz = (1 for i in [0...n])
  find:(x)->
    while @p[x] != x
      @p[x] = @p[@p[x]]
      x = @p[x]
    x
  unite:(a,b)->
    ra = @find a; rb = @find b
    return if ra == rb
    if @sz[ra] < @sz[rb] then [ra,rb] = [rb,ra]
    @p[rb] = ra
    @sz[ra] += @sz[rb]
  size:(x)-> @sz[@find x]

uf = new UF n
limit = Math.min 1000, edges.length
i = 0
while i < limit
  e = edges[i]
  uf.unite e.u, e.v
  i++

top = [0,0,0]
for i in [0...n] when uf.find(i) == i
  s = uf.size i
  if s > top[0]
    top[2] = top[1]; top[1] = top[0]; top[0] = s
  else if s > top[1]
    top[2] = top[1]; top[1] = s
  else if s > top[2]
    top[2] = s

product = 1
for k in [0...3] when top[k] > 0
  product *= top[k]

console.log "Product of three largest circuit sizes: #{product}"

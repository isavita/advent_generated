
fs = require 'fs'

grid = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
n = grid.length
m = grid[0].length

for i in [0...n]
  for j in [0...m]
    if grid[i][j] == 'S'
      sx = i
      sy = j
    else if grid[i][j] == 'E'
      ex = i
      ey = j

dx = [-1, 0, 1, 0]
dy = [0, 1, 0, -1]

dist = (()->
  d = []
  for i in [0...n]
    d[i] = []
    for j in [0...m]
      d[i][j] = [Infinity, Infinity, Infinity, Infinity]
  d
)()

dist[sx][sy][1] = 0

heap = [{x: sx, y: sy, d: 1, cost: 0}]

while heap.length > 0
  heap.sort (a, b) -> a.cost - b.cost
  u = heap.shift()

  if dist[u.x][u.y][u.d] < u.cost
    continue

  if u.x == ex and u.y == ey
    console.log u.cost
    process.exit(0)

  for ndir in [(u.d + 1) % 4, (u.d + 3) % 4]
    nc = u.cost + 1000
    if nc < dist[u.x][u.y][ndir]
      dist[u.x][u.y][ndir] = nc
      heap.push {x: u.x, y: u.y, d: ndir, cost: nc}

  nx = u.x + dx[u.d]
  ny = u.y + dy[u.d]

  if nx >= 0 and nx < n and ny >= 0 and ny < m and grid[nx][ny] != '#'
    nc = u.cost + 1
    if nc < dist[nx][ny][u.d]
      dist[nx][ny][u.d] = nc
      heap.push {x: nx, y: ny, d: u.d, cost: nc}

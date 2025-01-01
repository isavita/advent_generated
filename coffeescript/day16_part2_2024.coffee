
fs = require 'fs'
input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
n = input.length
m = input[0].length

dx = [-1, 0, 1, 0]
dy = [0, 1, 0, -1]

grid = input.map (row) -> row.split('')

findStartEnd = () ->
  sx = sy = ex = ey = -1
  for i in [0...n]
    for j in [0...m]
      if grid[i][j] == 'S'
        sx = i
        sy = j
      else if grid[i][j] == 'E'
        ex = i
        ey = j
  return {sx, sy, ex, ey}

{sx, sy, ex, ey} = findStartEnd()

dist = (()->
  d = []
  for i in [0...n]
    d[i] = []
    for j in [0...m]
      d[i][j] = [Infinity, Infinity, Infinity, Infinity]
  d
)()

dist[sx][sy][1] = 0

heap = []
push = (x, y, d, cost) ->
  heap.push {x, y, d, cost}
  heap.sort (a, b) -> a.cost - b.cost

pop = () -> heap.shift()

push sx, sy, 1, 0

while heap.length > 0
  u = pop()
  if dist[u.x][u.y][u.d] < u.cost
    continue
  for ndir in [(u.d + 1) % 4, (u.d + 3) % 4]
    nc = u.cost + 1000
    if nc < dist[u.x][u.y][ndir]
      dist[u.x][u.y][ndir] = nc
      push u.x, u.y, ndir, nc
  nx = u.x + dx[u.d]
  ny = u.y + dy[u.d]
  if nx >= 0 and nx < n and ny >= 0 and ny < m and grid[nx][ny] != '#'
    nc = u.cost + 1
    if nc < dist[nx][ny][u.d]
      dist[nx][ny][u.d] = nc
      push nx, ny, u.d, nc

best = Infinity
for d in [0...4]
  best = Math.min best, dist[ex][ey][d]

used = (()->
  u = []
  for i in [0...n]
    u[i] = []
    for j in [0...m]
      u[i][j] = false
  u
)()

rev = []
for d in [0...4]
  if dist[ex][ey][d] == best
    rev.push {x: ex, y: ey, d}

vis = (()->
  v = []
  for i in [0...n]
    v[i] = []
    for j in [0...m]
      v[i][j] = [false, false, false, false]
  v
)()

for s in rev
  vis[s.x][s.y][s.d] = true

while rev.length > 0
  u = rev.pop()
  used[u.x][u.y] = true
  costU = dist[u.x][u.y][u.d]
  for pd in [(u.d + 1) % 4, (u.d + 3) % 4]
    if dist[u.x][u.y][pd] == costU - 1000
      if not vis[u.x][u.y][pd]
        vis[u.x][u.y][pd] = true
        rev.push {x: u.x, y: u.y, d: pd}
  px = u.x - dx[u.d]
  py = u.y - dy[u.d]
  if px >= 0 and px < n and py >= 0 and py < m and grid[px][py] != '#'
    if dist[px][py][u.d] == costU - 1
      if not vis[px][py][u.d]
        vis[px][py][u.d] = true
        rev.push {x: px, y: py, d: u.d}

cnt = 0
for i in [0...n]
  for j in [0...m]
    if used[i][j] and grid[i][j] != '#'
      cnt++

console.log cnt

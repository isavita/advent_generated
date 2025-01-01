
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
h = input.length
w = input[0].length

S = null
E = null
walls = (for _ in [0...h] then (for _ in [0...w] then false))
trackCells = []

for i in [0...h]
  for j in [0...w]
    ch = input[i][j]
    if ch is 'S'
      S = {x: i, y: j}
    else if ch is 'E'
      E = {x: i, y: j}
    if ch is '#'
      walls[i][j] = true
    else
      trackCells.push {x: i, y: j}

dirs = [{x: 1, y: 0}, {x: -1, y: 0}, {x: 0, y: 1}, {x: 0, y: -1}]

isTrack = (x, y) ->
  x >= 0 and x < h and y >= 0 and y < w and not walls[x][y]

normalDistFrom = (start) ->
  dist = (for _ in [0...h] then (for _ in [0...w] then -1))
  dist[start.x][start.y] = 0
  q = [start]
  while q.length > 0
    cur = q.shift()
    for d in dirs
      nx = cur.x + d.x
      ny = cur.y + d.y
      continue if nx < 0 or nx >= h or ny < 0 or ny >= w
      continue if walls[nx][ny]
      if dist[nx][ny] < 0
        dist[nx][ny] = dist[cur.x][cur.y] + 1
        q.push {x: nx, y: ny}
  dist

distFromS = normalDistFrom(S)
distFromE = normalDistFrom(E)

if distFromS[E.x][E.y] < 0
  console.log 0
  return

normalCost = distFromS[E.x][E.y]

cheats = {}

for startPos in trackCells
  sd = distFromS[startPos.x][startPos.y]
  continue if sd < 0

  distC = (for _ in [0...h] then (for _ in [0...w] then -1))
  distC[startPos.x][startPos.y] = 0
  q = [startPos]

  while q.length > 0
    cur = q.shift()
    steps = distC[cur.x][cur.y]
    continue if steps is 20
    for d in dirs
      nx = cur.x + d.x
      ny = cur.y + d.y
      continue if nx < 0 or nx >= h or ny < 0 or ny >= w
      if distC[nx][ny] < 0
        distC[nx][ny] = steps + 1
        q.push {x: nx, y: ny}

  for x in [0...h]
    for y in [0...w]
      s = distC[x][y]
      if s > 0 and s <= 20 and isTrack(x,y)
        ed = distFromE[x][y]
        continue if ed < 0
        cost = sd + s + ed
        if cost < normalCost
          key = "#{startPos.x},#{startPos.y},#{x},#{y}"
          if not cheats[key] or cost < cheats[key]
            cheats[key] = cost

count = 0
for key, cost of cheats
  saving = normalCost - cost
  if saving >= 100
    count++

console.log count

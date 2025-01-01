
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
h = input.length
w = input[0].length

S = null
E = null
trackCells = []
walls = (new Array(h).fill(null).map(() => new Array(w).fill(false)))

for i in [0...h]
  for j in [0...w]
    ch = input[i][j]
    if ch == 'S'
      S = {x: i, y: j}
    else if ch == 'E'
      E = {x: i, y: j}
    if ch == '#'
      walls[i][j] = true
    else
      trackCells.push {x: i, y: j}

dirs = [{x: 1, y: 0}, {x: -1, y: 0}, {x: 0, y: 1}, {x: 0, y: -1}]

normalDistFrom = (start) ->
  dist = (new Array(h).fill(null).map(() => new Array(w).fill(-1)))
  dist[start.x][start.y] = 0
  q = [start]
  while q.length > 0
    cur = q.shift()
    for d in dirs
      nx = cur.x + d.x
      ny = cur.y + d.y
      if nx < 0 or nx >= h or ny < 0 or ny >= w
        continue
      if walls[nx][ny]
        continue
      if dist[nx][ny] == -1
        dist[nx][ny] = dist[cur.x][cur.y] + 1
        q.push {x: nx, y: ny}
  dist

distFromS = normalDistFrom(S)
distFromE = normalDistFrom(E)

if distFromS[E.x][E.y] == -1
  console.log 0
  return

normalCost = distFromS[E.x][E.y]

isTrack = (x, y) ->
  if x < 0 or x >= h or y < 0 or y >= w
    return false
  not walls[x][y]

possibleCheats = 0

for startPos in trackCells
  sd = distFromS[startPos.x][startPos.y]
  if sd == -1
    continue
  for d1 in dirs
    m1 = {x: startPos.x + d1.x, y: startPos.y + d1.y}
    if m1.x < 0 or m1.x >= h or m1.y < 0 or m1.y >= w
      continue
    for d2 in dirs
      m2 = {x: m1.x + d2.x, y: m1.y + d2.y}
      if m2.x < 0 or m2.x >= h or m2.y < 0 or m2.y >= w
        continue
      if not isTrack(m2.x, m2.y)
        continue
      ed = distFromE[m2.x][m2.y]
      if ed == -1
        continue
      newCost = sd + 2 + ed
      saving = normalCost - newCost
      if saving >= 100
        possibleCheats++

console.log possibleCheats

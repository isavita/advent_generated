
import sequtils, strutils, math, algorithm, heapqueue

type State = tuple[x, y, d: int]
type Node = tuple[x, y, d, cost: int]

proc solve() =
  let lines = readFile("input.txt").splitLines()
  let n = lines.len
  let m = lines[0].len
  var sx, sy, ex, ey: int
  for i in 0..<n:
    for j in 0..<m:
      if lines[i][j] == 'S':
        sx = i
        sy = j
      elif lines[i][j] == 'E':
        ex = i
        ey = j

  let dx = [-1, 0, 1, 0]
  let dy = [0, 1, 0, -1]

  var dist = newSeqWith(n, newSeqWith(m, newSeqWith(4, int.high)))
  dist[sx][sy][1] = 0

  var h = initHeapQueue[Node]()
  h.push((sx, sy, 1, 0))

  while h.len > 0:
    let u = h.pop()
    if dist[u.x][u.y][u.d] < u.cost:
      continue
    if u.x == ex and u.y == ey:
      continue
    for ndir in [(u.d + 1) mod 4, (u.d + 3) mod 4]:
      let nc = u.cost + 1000
      if nc < dist[u.x][u.y][ndir]:
        dist[u.x][u.y][ndir] = nc
        h.push((u.x, u.y, ndir, nc))
    let nx = u.x + dx[u.d]
    let ny = u.y + dy[u.d]
    if nx >= 0 and nx < n and ny >= 0 and ny < m and lines[nx][ny] != '#':
      let nc = u.cost + 1
      if nc < dist[nx][ny][u.d]:
        dist[nx][ny][u.d] = nc
        h.push((nx, ny, u.d, nc))

  var best = int.high
  for d in 0..<4:
    best = min(best, dist[ex][ey][d])

  var used = newSeqWith(n, newSeqWith(m, false))

  var rev = newSeq[State]()
  for d in 0..<4:
    if dist[ex][ey][d] == best:
      rev.add((ex, ey, d))

  var vis = newSeqWith(n, newSeqWith(m, newSeqWith(4, false)))
  for s in rev:
    vis[s.x][s.y][s.d] = true

  while rev.len > 0:
    let u = rev.pop()
    used[u.x][u.y] = true
    let costU = dist[u.x][u.y][u.d]
    for pd in [(u.d + 1) mod 4, (u.d + 3) mod 4]:
      if dist[u.x][u.y][pd] == costU - 1000:
        if not vis[u.x][u.y][pd]:
          vis[u.x][u.y][pd] = true
          rev.add((u.x, u.y, pd))
    let px = u.x - dx[u.d]
    let py = u.y - dy[u.d]
    if px >= 0 and px < n and py >= 0 and py < m and lines[px][py] != '#':
      if dist[px][py][u.d] == costU - 1:
        if not vis[px][py][u.d]:
          vis[px][py][u.d] = true
          rev.add((px, py, u.d))

  var cnt = 0
  for i in 0..<n:
    for j in 0..<m:
      if used[i][j] and lines[i][j] != '#':
        cnt += 1

  echo cnt

solve()

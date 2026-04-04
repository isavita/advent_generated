
#!/usr/bin/env bash
set -euo pipefail

awk '
BEGIN {
  maxX = 70
  maxY = 70
  n = 0
  while ((getline line < "input.txt") > 0) {
    split(line, p, ",")
    xs[n] = p[1] + 0
    ys[n] = p[2] + 0
    n++
  }
  close("input.txt")

  limit = (n < 1024 ? n : 1024)
  delete blocked
  for (i = 0; i < limit; i++) blocked[xs[i] "," ys[i]] = 1
  print bfs()

  delete blocked
  for (i = 0; i < n; i++) {
    blocked[xs[i] "," ys[i]] = 1
    if (bfs() == -1) {
      print xs[i] "," ys[i]
      exit
    }
  }
}

function bfs(   head, tail, x, y, d, nx, ny, key) {
  delete vis
  head = tail = 0
  qx[tail] = 0; qy[tail] = 0; qd[tail] = 0; tail++
  vis["0,0"] = 1

  while (head < tail) {
    x = qx[head]; y = qy[head]; d = qd[head]; head++

    if (x == maxX && y == maxY) return d

    nx = x + 1; ny = y
    if (valid(nx, ny)) {
      key = nx "," ny
      if (!(key in vis)) {
        vis[key] = 1
        qx[tail] = nx; qy[tail] = ny; qd[tail] = d + 1; tail++
      }
    }

    nx = x - 1; ny = y
    if (valid(nx, ny)) {
      key = nx "," ny
      if (!(key in vis)) {
        vis[key] = 1
        qx[tail] = nx; qy[tail] = ny; qd[tail] = d + 1; tail++
      }
    }

    nx = x; ny = y + 1
    if (valid(nx, ny)) {
      key = nx "," ny
      if (!(key in vis)) {
        vis[key] = 1
        qx[tail] = nx; qy[tail] = ny; qd[tail] = d + 1; tail++
      }
    }

    nx = x; ny = y - 1
    if (valid(nx, ny)) {
      key = nx "," ny
      if (!(key in vis)) {
        vis[key] = 1
        qx[tail] = nx; qy[tail] = ny; qd[tail] = d + 1; tail++
      }
    }
  }
  return -1
}

function valid(x, y, key) {
  if (x < 0 || x > maxX || y < 0 || y > maxY) return 0
  key = x "," y
  return !(key in blocked)
}
' input.txt

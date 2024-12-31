
import strutils, sequtils, os, algorithm

const size = 71

proc canReach(grid: seq[seq[bool]]): bool =
  let n = grid.len
  if grid[0][0] or grid[n-1][n-1]:
    return false
  const dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
  var visited = newSeqWith(n, newSeq[bool](n))
  var q: seq[(int, int)] = @[(0, 0)]
  visited[0][0] = true
  while q.len > 0:
    let c = q[0]
    q.delete(0)
    if c == (n - 1, n - 1):
      return true
    for d in dirs:
      let nx = c[0] + d[0]
      let ny = c[1] + d[1]
      if nx >= 0 and ny >= 0 and nx < n and ny < n and not grid[ny][nx] and not visited[ny][nx]:
        visited[ny][nx] = true
        q.add((nx, ny))
  return false

var grid = newSeqWith(size, newSeq[bool](size))
var f = open("input.txt")
var i = 0
for line in f.lines:
  let parts = line.split(",")
  let x = parseInt(parts[0])
  let y = parseInt(parts[1])
  if x >= 0 and x < size and y >= 0 and y < size:
    grid[y][x] = true
  i += 1
  if not canReach(grid):
    echo x, ",", y
    quit()
echo "No cutoff found"

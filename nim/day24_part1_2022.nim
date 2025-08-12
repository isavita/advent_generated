
import std/[os, strutils, sequtils, tables, sets, deques]

const
  dr = [0, 0, 1, -1, 0]   # Right, Left, Down, Up, Wait
  dc = [1, -1, 0, 0, 0]

proc gcd(a, b: int): int =
  var x = a; var y = b
  while y != 0:
    (x, y) = (y, x mod y)
  x

proc lcm(a, b: int): int = (a * b) div gcd(a, b)

func modPos(a, m: int): int =
  ## positive modulo (C‑style remainder)
  let r = a mod m
  if r < 0: r + m else: r

proc isBlizzard(grid: seq[string]; row, col, time, period: int): bool =
  let rows = grid.len
  let cols = grid[0].len
  let r = rows - 2
  let c = cols - 2

  # right‑moving ‘>’
  if grid[row][ ( (col - 1 - time) mod c + c) mod c + 1 ] == '>': return true
  # left‑moving ‘<’
  if grid[row][ ( (col - 1 + time) mod c + c) mod c + 1 ] == '<': return true
  # down‑moving ‘v’
  if grid[( (row - 1 - time) mod r + r) mod r + 1][col] == 'v': return true
  # up‑moving ‘^’
  if grid[( (row - 1 + time) mod r + r) mod r + 1][col] == '^': return true
  false

proc solve(grid: seq[string]; startR, startC, endR, endC: int): int =
  let rows = grid.len
  let cols = grid[0].len
  let period = lcm(rows - 2, cols - 2)

  var visited = initHashSet[(int, int, int)]()
  var q = initDeque[(int, int, int)]()   # (row, col, time)

  q.addLast((startR, startC, 0))
  visited.incl((startR, startC, 0))

  while q.len > 0:
    let (r, c, t) = q.popFirst()
    if r == endR and c == endC:
      return t

    for i in 0..4:
      let nr = r + dr[i]
      let nc = c + dc[i]
      let nt = t + 1
      if nr < 0 or nr >= rows or nc < 0 or nc >= cols: continue
      if grid[nr][nc] == '#': continue
      if isBlizzard(grid, nr, nc, nt, period): continue

      let modt = nt mod period
      let state = (nr, nc, modt)
      if state notin visited:
        visited.incl(state)
        q.addLast((nr, nc, nt))

  -1   # unreachable (input guarantees a solution)

proc main() =
  let lines = readFile("input.txt").splitLines()
  var grid = newSeq[string](lines.len)
  for i, line in lines:
    grid[i] = line

  var startCol = -1
  for i, ch in grid[0]:
    if ch == '.':
      startCol = i
      break

  var endCol = -1
  let lastRow = grid.len - 1
  for i, ch in grid[lastRow]:
    if ch == '.':
      endCol = i
      break

  let minutes = solve(grid, 0, startCol, lastRow, endCol)
  echo minutes

when isMainModule:
  main()

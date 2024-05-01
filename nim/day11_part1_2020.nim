import sequtils, strutils, algorithm

type
  Seat = enum L, O, F
  Grid = seq[seq[Seat]]

proc `==`(a, b: Grid): bool =
  if a.len != b.len:
    return false
  for i in 0..<a.len:
    if a[i] != b[i]:
      return false
  true

proc countOccupied(g: Grid): int =
  for row in g:
    for seat in row:
      if seat == O:
        result.inc

proc countAdjacent(g: Grid, x, y: int): int =
  for dx in -1..1:
    for dy in -1..1:
      if dx == 0 and dy == 0:
        continue
      let nx = x + dx
      let ny = y + dy
      if nx >= 0 and nx < g.len and ny >= 0 and ny < g[0].len:
        if g[nx][ny] == O:
          result.inc

proc step(g: Grid): Grid =
  result = newSeq[seq[Seat]](g.len)
  for i in 0..<g.len:
    result[i] = newSeq[Seat](g[i].len)
    for j in 0..<g[i].len:
      let adjacent = countAdjacent(g, i, j)
      case g[i][j]
      of L:
        if adjacent == 0:
          result[i][j] = O
        else:
          result[i][j] = L
      of O:
        if adjacent >= 4:
          result[i][j] = L
        else:
          result[i][j] = O
      of F:
        result[i][j] = F

when isMainModule:
  let input = readFile("input.txt").strip().splitLines()
  var grid: Grid
  for line in input:
    grid.add(newSeq[Seat](line.len))
    for i, c in line:
      case c
      of 'L': grid[^1][i] = L
      of '#': grid[^1][i] = O
      of '.': grid[^1][i] = F
      else: discard

  var prevGrid = grid
  while true:
    let newGrid = step(prevGrid)
    if newGrid == prevGrid:
      break
    prevGrid = newGrid

  echo "Occupied seats: ", countOccupied(prevGrid)
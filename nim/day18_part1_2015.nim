import sequtils, strutils

const gridSize = 100
const steps = 100

proc countOnNeighbors(grid: seq[seq[bool]], x, y: int): int =
  var on = 0
  for dx in -1..1:
    for dy in -1..1:
      if dx == 0 and dy == 0:
        continue
      let nx = x + dx
      let ny = y + dy
      if nx >= 0 and nx < gridSize and ny >= 0 and ny < gridSize and grid[nx][ny]:
        inc on
  on

proc step(grid: seq[seq[bool]]): seq[seq[bool]] =
  var newGrid = newSeqWith(gridSize, newSeq[bool](gridSize))
  for x in 0..<gridSize:
    for y in 0..<gridSize:
      let onNeighbors = countOnNeighbors(grid, x, y)
      if grid[x][y]:
        newGrid[x][y] = onNeighbors == 2 or onNeighbors == 3
      else:
        newGrid[x][y] = onNeighbors == 3
  newGrid

when isMainModule:
  let file = readFile("input.txt")
  var grid = newSeqWith(gridSize, newSeq[bool](gridSize))
  var y = 0
  for line in file.splitLines():
    for x, c in line:
      grid[x][y] = c == '#'
    inc y

  for _ in 0..<steps:
    grid = step(grid)

  var onCount = 0
  for row in grid:
    for light in row:
      if light:
        inc onCount

  echo onCount
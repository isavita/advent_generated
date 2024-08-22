import strutils, sequtils, math

type Direction = enum
  Right, Up, Left, Down

proc getAdjacentSum(grid: seq[seq[int]], x, y: int): int =
  result = 0
  for dx in -1..1:
    for dy in -1..1:
      if dx == 0 and dy == 0:
        continue
      if x + dx >= 0 and x + dx < grid.len and y + dy >= 0 and y + dy < grid[0].len:
        result += grid[x + dx][y + dy]

proc spiralMemory(input: int): int =
  let size = 100
  var grid = newSeqWith(size, newSeq[int](size))
  var x = size div 2
  var y = size div 2
  var dir = Right
  var step = 1
  var steps = 0
  var changeDir = false

  grid[x][y] = 1
  while true:
    case dir
    of Right: x += 1
    of Up: y -= 1
    of Left: x -= 1
    of Down: y += 1

    grid[x][y] = getAdjacentSum(grid, x, y)
    if grid[x][y] > input:
      return grid[x][y]

    steps += 1
    if steps == step:
      steps = 0
      if changeDir:
        step += 1
      changeDir = not changeDir
      case dir
      of Right: dir = Up
      of Up: dir = Left
      of Left: dir = Down
      of Down: dir = Right

when isMainModule:
  let input = readFile("input.txt").strip.parseInt
  echo spiralMemory(input)
import strutils, sequtils, algorithm

proc readInput(filename: string): seq[seq[int]] =
  var grid: seq[seq[int]]
  for line in lines(filename):
    var row: seq[int]
    for ch in line:
      row.add parseInt($ch)
    grid.add row
  grid

proc isVisible(grid: seq[seq[int]], x, y: int): bool =
  let height = grid[x][y]
  var visible = true
  for i in 0..<x:
    if grid[i][y] >= height:
      visible = false
      break
  if visible: return true
  visible = true
  for i in x+1..<grid.len:
    if grid[i][y] >= height:
      visible = false
      break
  if visible: return true
  visible = true
  for i in 0..<y:
    if grid[x][i] >= height:
      visible = false
      break
  if visible: return true
  visible = true
  for i in y+1..<grid[x].len:
    if grid[x][i] >= height:
      visible = false
      break
  return visible

proc part1(grid: seq[seq[int]]): int =
  var count = 0
  for x in 0..<grid.len:
    for y in 0..<grid[x].len:
      if isVisible(grid, x, y):
        inc count
  count

proc scenicScore(grid: seq[seq[int]], x, y: int): int =
  let height = grid[x][y]
  var up = 0
  for i in countdown(x-1, 0):
    inc up
    if grid[i][y] >= height: break
  var down = 0
  for i in x+1..<grid.len:
    inc down
    if grid[i][y] >= height: break
  var left = 0
  for i in countdown(y-1, 0):
    inc left
    if grid[x][i] >= height: break
  var right = 0
  for i in y+1..<grid[x].len:
    inc right
    if grid[x][i] >= height: break
  up * down * left * right

proc part2(grid: seq[seq[int]]): int =
  var maxScore = 0
  for x in 0..<grid.len:
    for y in 0..<grid[x].len:
      let score = scenicScore(grid, x, y)
      if score > maxScore:
        maxScore = score
  maxScore

let grid = readInput("input.txt")
echo "Part 1: ", part1(grid)
echo "Part 2: ", part2(grid)
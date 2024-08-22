import sequtils
import strutils

proc isVisible(grid: seq[seq[int]], row, col: int): bool =
  let height = grid[row][col]
  var visible = true

  for i in 0..<row:
    if grid[i][col] >= height:
      visible = false
      break
  if visible: return true

  visible = true
  for i in row+1..<grid.len:
    if grid[i][col] >= height:
      visible = false
      break
  if visible: return true

  visible = true
  for i in 0..<col:
    if grid[row][i] >= height:
      visible = false
      break
  if visible: return true

  visible = true
  for i in col+1..<grid[row].len:
    if grid[row][i] >= height:
      visible = false
      break
  if visible: return true

  return false

proc countVisibleTrees(filename: string): int =
  let file = readFile(filename)
  let lines = file.splitLines()
  var grid: seq[seq[int]] = @[]

  for line in lines:
    var row: seq[int] = @[]
    for ch in line:
      row.add(int(ch) - int('0'))
    grid.add(row)

  var visibleCount = 0

  for row in 0..<grid.len:
    for col in 0..<grid[row].len:
      if row == 0 or col == 0 or row == grid.len-1 or col == grid[row].len-1:
        visibleCount += 1
      elif isVisible(grid, row, col):
        visibleCount += 1

  return visibleCount

echo countVisibleTrees("input.txt")
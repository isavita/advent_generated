
import strutils, sequtils, os, system

type Grid = seq[seq[char]]

proc countAdjacent(r, c: int, grid: Grid): (int, int, int) = # (open, wooded, lumber)
  var woodedCount, lumberCount = 0
  let rows = grid.len
  let cols = grid[0].len
  for i in max(0, r - 1)..min(rows - 1, r + 1):
    for j in max(0, c - 1)..min(cols - 1, c + 1):
      if i == r and j == c: continue
      case grid[i][j]
      of '|': inc woodedCount
      of '#': inc lumberCount
      else: discard
  # No need to count '.', it's implicit
  return (0, woodedCount, lumberCount) # Returning 0 for open as it's not used directly in rules

proc updateArea(grid: Grid): Grid =
  let rows = grid.len
  let cols = grid[0].len
  result = newSeq[seq[char]](rows)
  for r in 0..<rows:
    result[r] = newSeq[char](cols)
    for c in 0..<cols:
      let (_, woodedAdj, lumberAdj) = countAdjacent(r, c, grid)
      case grid[r][c]
      of '.':
        result[r][c] = if woodedAdj >= 3: '|' else: '.'
      of '|':
        result[r][c] = if lumberAdj >= 3: '#' else: '|'
      of '#':
        result[r][c] = if lumberAdj >= 1 and woodedAdj >= 1: '#' else: '.'
      else: discard

proc resourceValue(grid: Grid): int =
  var wooded = 0
  var lumberyards = 0
  for row in grid:
    for cell in row:
      if cell == '|': inc wooded
      elif cell == '#': inc lumberyards
  return wooded * lumberyards

proc readInput(filename: string): Grid =
  let content = readFile(filename)
  let lines = content.strip.splitLines
  result = newSeq[seq[char]](lines.len)
  for i, line in lines:
    result[i] = toSeq(line) # Efficiently convert string to seq[char]

when isMainModule:
  var grid = readInput("input.txt")
  for i in 1..10:
    grid = updateArea(grid)
  echo resourceValue(grid)

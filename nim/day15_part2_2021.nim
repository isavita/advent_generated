import std/[sequtils, strutils, algorithm, strformat, tables, math]

type
  Point = object
    x, y: int
  Grid = seq[seq[int]]

proc parseInput(filename: string): Grid =
  let file = readFile(filename)
  let lines = file.splitLines()
  var grid: Grid = newSeq[seq[int]](len(lines))
  for i, line in lines:
    grid[i] = newSeq[int](len(line))
    for j, char in line:
      grid[i][j] = parseInt($char)
  result = grid

proc expandGrid(grid: Grid, factor: int): Grid =
  let originalSize = grid.len
  let newSize = originalSize * factor
  var expandedGrid: Grid = newSeq[seq[int]](newSize)
  for i in 0..<newSize:
    expandedGrid[i] = newSeq[int](newSize)
  for i in 0..<originalSize:
    for j in 0..<originalSize:
      for k in 0..<factor:
        for l in 0..<factor:
          var newValue = grid[i][j] + k + l
          if newValue > 9:
            newValue -= 9
          expandedGrid[i + k * originalSize][j + l * originalSize] = newValue
  result = expandedGrid

proc findLowestRiskPath(grid: Grid): int =
  let directions = [Point(x: 1, y: 0), Point(x: 0, y: 1), Point(x: -1, y: 0), Point(x: 0, y: -1)]
  var riskLevels: seq[seq[int]] = newSeq[seq[int]](grid.len)
  for i in 0..<grid.len:
    riskLevels[i] = newSeq[int](grid[i].len)
    for j in 0..<grid[i].len:
      riskLevels[i][j] = int.high
  riskLevels[0][0] = 0
  var queue: seq[Point] = @[]
  queue.add(Point(x: 0, y: 0))
  while queue.len > 0:
    let current = queue[0]
    queue.delete(0)
    for dir in directions:
      let newX = current.x + dir.x
      let newY = current.y + dir.y
      if newX >= 0 and newX < grid.len and newY >= 0 and newY < grid[0].len:
        let newRisk = riskLevels[current.x][current.y] + grid[newX][newY]
        if newRisk < riskLevels[newX][newY]:
          riskLevels[newX][newY] = newRisk
          queue.add(Point(x: newX, y: newY))
  result = riskLevels[grid.len - 1][grid[0].len - 1]

let grid = parseInput("input.txt")
let expandedGrid = expandGrid(grid, 5)
let lowestRisk = findLowestRiskPath(expandedGrid)
echo fmt"Lowest total risk: {lowestRisk}"
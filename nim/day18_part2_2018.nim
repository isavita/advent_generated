import std/[sequtils, strutils, tables]

type Acre = enum Open, Trees, Lumberyard

proc parseInput(filename: string): seq[seq[Acre]] =
  let file = readFile(filename)
  var grid: seq[seq[Acre]] = @[]
  for line in file.splitLines:
    var row: seq[Acre] = @[]
    for char in line:
      case char
      of '.': row.add(Acre.Open)
      of '|': row.add(Acre.Trees)
      of '#': row.add(Acre.Lumberyard)
      else: discard
    grid.add(row)
  grid

proc countAdjacent(grid: seq[seq[Acre]], x, y: int, acreType: Acre): int =
  var count = 0
  for dx in -1..1:
    for dy in -1..1:
      if dx == 0 and dy == 0:
        continue
      let nx = x + dx
      let ny = y + dy
      if nx >= 0 and nx < grid.len and ny >= 0 and ny < grid[0].len:
        if grid[nx][ny] == acreType:
          count.inc
  count

proc simulate(grid: seq[seq[Acre]]): seq[seq[Acre]] =
  var newGrid: seq[seq[Acre]] = @[]
  for x in 0..<grid.len:
    var newRow: seq[Acre] = @[]
    for y in 0..<grid[x].len:
      let openCount = countAdjacent(grid, x, y, Acre.Open)
      let treesCount = countAdjacent(grid, x, y, Acre.Trees)
      let lumberyardCount = countAdjacent(grid, x, y, Acre.Lumberyard)
      case grid[x][y]
      of Acre.Open:
        if treesCount >= 3:
          newRow.add(Acre.Trees)
        else:
          newRow.add(Acre.Open)
      of Acre.Trees:
        if lumberyardCount >= 3:
          newRow.add(Acre.Lumberyard)
        else:
          newRow.add(Acre.Trees)
      of Acre.Lumberyard:
        if lumberyardCount >= 1 and treesCount >= 1:
          newRow.add(Acre.Lumberyard)
        else:
          newRow.add(Acre.Open)
    newGrid.add(newRow)
  newGrid

proc calculateResourceValue(grid: seq[seq[Acre]]): int =
  var treesCount = 0
  var lumberyardCount = 0
  for row in grid:
    for acre in row:
      case acre
      of Acre.Trees: treesCount.inc
      of Acre.Lumberyard: lumberyardCount.inc
      else: discard
  treesCount * lumberyardCount

proc findCycle(grid: seq[seq[Acre]], minutes: int): int =
  var seen: Table[string, int] = initTable[string, int]()
  var cycleStart = 0
  var cycleLength = 0
  var gridCopy = grid
  for minute in 0..<minutes:
    let gridStr = $gridCopy
    if gridStr in seen:
      cycleStart = seen[gridStr]
      cycleLength = minute - cycleStart
      break
    seen[gridStr] = minute
    gridCopy = simulate(gridCopy)
  if cycleLength > 0:
    let remainingMinutes = (minutes - cycleStart) mod cycleLength
    for _ in 0..<remainingMinutes:
      gridCopy = simulate(gridCopy)
  calculateResourceValue(gridCopy)

when isMainModule:
  let grid = parseInput("input.txt")
  let resourceValue = findCycle(grid, 1000000000)
  echo resourceValue
fs = require 'fs'

Open = '.'
Trees = '|'
Lumberyard = '#'
Size = 50

readInput = (filename) ->
  grid = []
  data = fs.readFileSync(filename, 'utf8')
  data.split('\n').forEach (line) ->
    row = []
    line.split('').forEach (char) ->
      row.push char
    grid.push row
  return grid

transform = (grid) ->
  newGrid = []
  grid.forEach (row, i) ->
    newRow = []
    row.forEach (cell, j) ->
      newRow.push nextAcreState(grid, i, j)
    newGrid.push newRow
  return newGrid

nextAcreState = (grid, i, j) ->
  switch grid[i][j]
    when Open
      if countAdjacent(grid, i, j, Trees) >= 3
        return Trees
    when Trees
      if countAdjacent(grid, i, j, Lumberyard) >= 3
        return Lumberyard
    when Lumberyard
      if countAdjacent(grid, i, j, Lumberyard) >= 1 and countAdjacent(grid, i, j, Trees) >= 1
        return Lumberyard
      return Open
  return grid[i][j]

countAdjacent = (grid, i, j, acreType) ->
  count = 0
  for x in [-1..1]
    for y in [-1..1]
      if x == 0 and y == 0
        continue
      if i + x >= 0 and i + x < grid.length and j + y >= 0 and j + y < grid[i].length
        if grid[i + x][j + y] == acreType
          count++
  return count

countResources = (grid) ->
  wooded = 0
  lumberyards = 0
  grid.forEach (row) ->
    row.forEach (cell) ->
      switch cell
        when Trees
          wooded++
        when Lumberyard
          lumberyards++
  return [wooded, lumberyards]

gridToString = (grid) ->
  str = ''
  grid.forEach (row) ->
    str += row.join('') + '\n'
  return str

grid = readInput('input.txt')
seenStates = {}
cycleStart = 0
cycleLength = 0
minute = 0
while true
  state = gridToString(grid)
  if seenMinute = seenStates[state]
    cycleStart = seenMinute
    cycleLength = minute - seenMinute
    break
  seenStates[state] = minute
  grid = transform(grid)
  minute++

remainingMinutes = (1000000000 - cycleStart) % cycleLength
for i in [0...remainingMinutes]
  grid = transform(grid)

[wooded, lumberyards] = countResources(grid)
console.log wooded * lumberyards
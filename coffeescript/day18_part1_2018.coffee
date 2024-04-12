fs = require 'fs'

Open = '.'
Trees = '|'
Lumberyard = '#'
Size = 50

transform = (grid) ->
  newGrid = []
  for i in [0...grid.length]
    newGrid.push []
    for j in [0...grid[i].length]
      newGrid[i].push nextAcreState(grid, i, j)
  newGrid

nextAcreState = (grid, i, j) ->
  switch grid[i][j]
    when Open
      if countAdjacent(grid, i, j, Trees) >= 3 then Trees else Open
    when Trees
      if countAdjacent(grid, i, j, Lumberyard) >= 3 then Lumberyard else Trees
    when Lumberyard
      if countAdjacent(grid, i, j, Lumberyard) >= 1 and countAdjacent(grid, i, j, Trees) >= 1
        Lumberyard
      else
        Open

countAdjacent = (grid, i, j, acreType) ->
  count = 0
  for x in [-1..1]
    for y in [-1..1]
      nextX = i + x
      nextY = j + y
      if x == 0 and y == 0 or nextX < 0 or nextX >= grid.length or nextY < 0 or nextY >= grid[i].length
        continue
      count++ if grid[nextX][nextY] == acreType
  count

countResources = (grid) ->
  wooded = 0
  lumberyards = 0
  for i in [0...grid.length]
    for j in [0...grid[i].length]
      wooded++ if grid[i][j] == Trees
      lumberyards++ if grid[i][j] == Lumberyard
  [wooded, lumberyards]

readInput = (filename) ->
  data = fs.readFileSync filename, 'utf8'
  grid = []
  lines = data.split '\n'
  for line in lines
    row = []
    for char in line
      row.push char
    grid.push row
  grid

grid = readInput 'input.txt'
for minute in [0...10]
  grid = transform grid
[wooded, lumberyards] = countResources grid
console.log wooded * lumberyards
fs = require 'fs'

gridSize = 100
steps = 100

countOnNeighbors = (grid, x, y) ->
  count = 0
  for dx in [-1, 0, 1]
    for dy in [-1, 0, 1]
      continue if dx == 0 and dy == 0
      nx = x + dx
      ny = y + dy
      if nx >= 0 and nx < gridSize and ny >= 0 and ny < gridSize and grid[nx][ny]
        count++
  count

step = (grid) ->
  newGrid = Array(gridSize).fill().map -> Array(gridSize).fill(false)
  for x in [0...gridSize]
    for y in [0...gridSize]
      onNeighbors = countOnNeighbors(grid, x, y)
      if grid[x][y]
        newGrid[x][y] = onNeighbors == 2 or onNeighbors == 3
      else
        newGrid[x][y] = onNeighbors == 3
  newGrid

data = fs.readFileSync 'input.txt', 'utf8'
lines = data.trim().split '\n'
grid = Array(gridSize).fill().map -> Array(gridSize).fill(false)

for y in [0...lines.length]
  line = lines[y]
  for x in [0...line.length]
    grid[x][y] = line[x] == '#'

for i in [0...steps]
  grid = step(grid)

onCount = 0
for row in grid
  for light in row
    onCount++ if light

console.log onCount
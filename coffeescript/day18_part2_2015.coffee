fs = require 'fs'

gridSize = 100
steps = 100

countOnNeighbors = (grid, x, y) ->
  count = 0
  for dx in [-1..1]
    for dy in [-1..1]
      continue if dx == 0 and dy == 0
      nx = x + dx
      ny = y + dy
      if nx >= 0 and nx < gridSize and ny >= 0 and ny < gridSize and grid[nx][ny]
        count++
  count

step = (grid) ->
  newGrid = (Array(gridSize).fill().map -> Array(gridSize).fill(false))
  for x in [0...gridSize]
    for y in [0...gridSize]
      onNeighbors = countOnNeighbors(grid, x, y)
      if grid[x][y]
        newGrid[x][y] = onNeighbors == 2 or onNeighbors == 3
      else
        newGrid[x][y] = onNeighbors == 3

  # Ensure corners are always on
  newGrid[0][0] = true
  newGrid[0][gridSize-1] = true
  newGrid[gridSize-1][0] = true
  newGrid[gridSize-1][gridSize-1] = true

  newGrid

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error opening file:", err
    return

  grid = Array(gridSize).fill().map -> Array(gridSize).fill(false)
  lines = data.split '\n'

  for y in [0...lines.length]
    line = lines[y]
    for x in [0...line.length]
      grid[x][y] = line[x] == '#'

  # Initialize corners as always on
  grid[0][0] = true
  grid[0][gridSize-1] = true
  grid[gridSize-1][0] = true
  grid[gridSize-1][gridSize-1] = true

  for i in [0...steps]
    grid = step(grid)

  onCount = 0
  for row in grid
    for light in row
      onCount++ if light

  console.log onCount
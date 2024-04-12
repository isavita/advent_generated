fs = require 'fs'

simulateStep = (grid) ->
  flashes = 0
  flashed = {}

  # Increase energy by 1 for all octopuses
  for y in [0...grid.length]
    for x in [0...grid[y].length]
      grid[y][x] += 1

  # Flash octopuses with energy greater than 9
  for y in [0...grid.length]
    for x in [0...grid[y].length]
      if grid[y][x] > 9
        flashes += flash(grid, x, y, flashed)

  # Reset energy to 0 for all that flashed
  for key, _ of flashed
    [x, y] = key.split(',').map (n) -> parseInt(n, 10)
    grid[y][x] = 0

  flashes

flash = (grid, x, y, flashed) ->
  key = "#{x},#{y}"
  return 0 if flashed[key]

  flashed[key] = true
  flashes = 1
  directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]

  for dir in directions
    newX = x + dir[0]
    newY = y + dir[1]
    if newX >= 0 and newX < grid[0].length and newY >= 0 and newY < grid.length
      grid[newY][newX] += 1
      if grid[newY][newX] > 9
        flashes += flash(grid, newX, newY, flashed)

  flashes

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  grid = data.trim().split('\n').map (line) -> line.split('').map (char) -> parseInt(char, 10)

  step = 0
  while true
    step++
    flashes = simulateStep(grid)
    break if flashes == 100 # All octopuses have flashed

  console.log step
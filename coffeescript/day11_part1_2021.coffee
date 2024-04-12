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
    [x, y] = key.split(',').map(Number)
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

readInput = (filename) ->
  data = fs.readFileSync(filename, 'utf8')
  grid = (line.split('').map(Number) for line in data.trim().split('\n'))
  grid

main = ->
  grid = readInput('input.txt')
  totalFlashes = 0
  for step in [0...100]
    totalFlashes += simulateStep(grid)

  console.log totalFlashes

main()
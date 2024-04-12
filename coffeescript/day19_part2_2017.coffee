fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  grid = data.split '\n'
  grid.pop() if grid[grid.length - 1] == ''

  x = y = 0
  for i in [0...grid[0].length]
    if grid[0][i] == '|'
      x = i
      break

  dx = 0
  dy = 1
  steps = 0

  while true
    break if x < 0 or x >= grid[0].length or y < 0 or y >= grid.length

    cell = grid[y][x]

    break if cell == ' '

    steps++

    if cell == '+'
      if dx == 0
        if x > 0 and (grid[y][x-1] == '-' or (grid[y][x-1] >= 'A' and grid[y][x-1] <= 'Z'))
          dx = -1
          dy = 0
        else
          dx = 1
          dy = 0
      else
        if y > 0 and (grid[y-1][x] == '|' or (grid[y-1][x] >= 'A' and grid[y-1][x] <= 'Z'))
          dx = 0
          dy = -1
        else
          dx = 0
          dy = 1

    x += dx
    y += dy

  console.log steps
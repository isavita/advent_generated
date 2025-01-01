
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
grid = input.map (line) -> line.split ''
height = grid.length
width = grid[0].length

moveEast = () ->
  moved = false
  newGrid = grid.map (row) -> row.slice()
  for y in [0...height]
    for x in [0...width]
      if grid[y][x] is '>'
        nextX = (x + 1) % width
        if grid[y][nextX] is '.'
          newGrid[y][nextX] = '>'
          newGrid[y][x] = '.'
          moved = true
  grid = newGrid
  moved

moveSouth = () ->
  moved = false
  newGrid = grid.map (row) -> row.slice()
  for x in [0...width]
    for y in [0...height]
      if grid[y][x] is 'v'
        nextY = (y + 1) % height
        if grid[nextY][x] is '.'
          newGrid[nextY][x] = 'v'
          newGrid[y][x] = '.'
          moved = true
  grid = newGrid
  moved

step = 0
while true
  eastMoved = moveEast()
  southMoved = moveSouth()
  step++
  if not eastMoved and not southMoved
    break

console.log step

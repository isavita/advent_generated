fs = require 'fs'

Side = 5
Square = Side * Side

parse = ->
  res = new Array(Square).fill(false)
  lines = fs.readFileSync('input.txt', 'utf8').split('\n')
  for line, row in lines
    for col in [0...Side]
      res[row * Side + col] = line[col] == '#'
  res

next1 = (grid) ->
  newGrid = new Array(Square).fill(false)
  for i in [0...Square]
    row = Math.floor(i / Side)
    col = i % Side
    neighbours = 0
    if row > 0
      neighbours++ if grid[i - Side]
    if row < Side - 1
      neighbours++ if grid[i + Side]
    if col > 0
      neighbours++ if grid[i - 1]
    if col < Side - 1
      neighbours++ if grid[i + 1]
    if grid[i] && neighbours != 1
      newGrid[i] = false
    else if !grid[i] && (neighbours == 1 || neighbours == 2)
      newGrid[i] = true
    else
      newGrid[i] = grid[i]
  newGrid

biodiversity = (grid) ->
  bio = 0
  for i in [0...Square]
    bio += 1 << i if grid[i]
  bio

appeared = {}

grid = parse()
appeared[grid.toString()] = true
loop
  grid = next1(grid)
  if appeared[grid.toString()]
    console.log biodiversity(grid)
    break
  appeared[grid.toString()] = true
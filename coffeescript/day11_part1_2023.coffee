
fs = require 'fs'

readFile = (fileName) ->
  fs.readFileSync(fileName, 'utf-8').trim().split('\n')

buildGrid = (input, empty) ->
  width: input[0].length
  height: input.length
  data: (()->
    data = {}
    for y, line of input
      for x, char of line
        if char != empty
          data[[x,y]] = char
    data
  )()

getEmptyRows = (grid) ->
  emptyRows = []
  for y in [0...grid.height]
    isEmpty = true
    for x in [0...grid.width]
      if grid.data[[x,y]]?
        isEmpty = false
        break
    if isEmpty
      emptyRows.push y
  emptyRows

getEmptyCols = (grid) ->
  emptyCols = []
  for x in [0...grid.width]
    isEmpty = true
    for y in [0...grid.height]
      if grid.data[[x,y]]?
        isEmpty = false
        break
    if isEmpty
      emptyCols.push x
  emptyCols

calculateOffsets = (emptyIndexes, bound) ->
  offsets = (0 for _ in [0...bound])
  for idx in emptyIndexes
    for i in [idx + 1...offsets.length]
      offsets[i]++
  offsets

expandGrid = (grid, expansionFactor) ->
  emptyCols = getEmptyCols grid
  emptyRows = getEmptyRows grid
  numLinesToAdd = expansionFactor - 1

  newGrid =
    width: grid.width + emptyCols.length * numLinesToAdd
    height: grid.height + emptyRows.length * numLinesToAdd
    data: {}

  dXs = calculateOffsets emptyCols, grid.width
  dYs = calculateOffsets emptyRows, grid.height

  for y in [0...grid.height]
    for x in [0...grid.width]
      if grid.data[[x,y]]?
        newGrid.data[[x + dXs[x] * numLinesToAdd, y + dYs[y] * numLinesToAdd]] = grid.data[[x,y]]
  newGrid

abs = (x) -> if x < 0 then -x else x

calculateLength = (grid, c1, c2) ->
  dX = abs c2[0] - c1[0]
  dY = abs c2[1] - c1[1]
  dX + dY

solve = (input) ->
  grid = buildGrid input, '.'
  expandedGrid = expandGrid grid, 2

  res = 0
  coords = Object.keys(expandedGrid.data)
  for i in [0...coords.length]
    for j in [0...i]
      c1 = coords[i].split(',').map(Number)
      c2 = coords[j].split(',').map(Number)
      res += calculateLength expandedGrid, c1, c2
  res

input = readFile 'input.txt'
console.log solve input

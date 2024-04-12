fs = require 'fs'

class Coord
  constructor: (@x, @y) ->

class Grid
  constructor: (@width, @height) ->
    @data = {}

  add: (coord, value) ->
    @data[coord.x + ',' + coord.y] = value

  get: (coord) ->
    @data[coord.x + ',' + coord.y]

  emptyRows: ->
    emptyRows = []
    for y in [0...@height]
      isEmpty = true
      for x in [0...@width]
        coord = new Coord x, y
        if @get(coord)
          isEmpty = false
          break
      emptyRows.push y if isEmpty
    emptyRows

  emptyCols: ->
    emptyCols = []
    for x in [0...@width]
      isEmpty = true
      for y in [0...@height]
        coord = new Coord x, y
        if @get(coord)
          isEmpty = false
          break
      emptyCols.push x if isEmpty
    emptyCols

  calculateOffsets: (emptyIndexes, bound) ->
    offsets = Array(bound).fill(0)
    for idx in emptyIndexes
      for i in [idx+1...offsets.length]
        offsets[i]++
    offsets

  expand: (expansionFactor) ->
    emptyCols = @emptyCols()
    emptyRows = @emptyRows()
    numLinesToAdd = expansionFactor - 1

    newGrid = new Grid @width + emptyCols.length * numLinesToAdd, @height + emptyRows.length * numLinesToAdd

    dXs = @calculateOffsets emptyCols, @width
    dYs = @calculateOffsets emptyRows, @height

    for y in [0...@height]
      for x in [0...@width]
        coord = new Coord x, y
        value = @get(coord)
        if value
          newCoord = new Coord x + dXs[x] * numLinesToAdd, y + dYs[y] * numLinesToAdd
          newGrid.add newCoord, value
    newGrid

abs = (x) -> if x < 0 then -x else x

calculateLength = (grid, c1, c2) ->
  dX = abs c2.x - c1.x
  dY = abs c2.y - c1.y
  dX + dY

solve = (input, expansionFactor) ->
  grid = new Grid input[0].length, input.length
  for y in [0...input.length]
    for x in [0...input[y].length]
      if input[y][x] != '.'
        grid.add new Coord(x, y), input[y][x]

  expandedGrid = grid.expand expansionFactor

  res = 0
  alreadySeen = {}
  for key, value of expandedGrid.data
    [x1, y1] = key.split(',')
    coord1 = new Coord parseInt(x1), parseInt(y1)
    for seenKey, _ of alreadySeen
      [x2, y2] = seenKey.split(',')
      coord2 = new Coord parseInt(x2), parseInt(y2)
      length = calculateLength expandedGrid, coord1, coord2
      res += length
    alreadySeen[key] = true

  res

readFile = (fileName) ->
  fs.readFileSync(fileName, 'utf8').trim().split '\n'

input = readFile 'input.txt'
console.log solve(input, 1000000)
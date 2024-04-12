fs = require 'fs'

solve = (input) ->
  tiles = parseTilesFromInput input
  edgeSize = Math.sqrt(tiles.length)

  assembledTiles = backtrackAssemble tiles, null, {}

  product = assembledTiles[0][0].id
  product *= assembledTiles[0][edgeSize-1].id
  product *= assembledTiles[edgeSize-1][0].id
  product *= assembledTiles[edgeSize-1][edgeSize-1].id
  product

class Tile
  constructor: (@contents, @id) ->

parseTilesFromInput = (input) ->
  ans = []
  for block in input.split '\n\n'
    split = block.split '\n'
    tileID = parseInt split[0].match(/Tile (\d+):/)[1]

    contents = []
    contents.push line.split '' for line in split[1..]
    ans.push new Tile(contents, tileID)
  ans

backtrackAssemble = (tiles, assembledTiles, usedIndices) ->
  edgeSize = Math.sqrt(tiles.length)
  unless assembledTiles
    assembledTiles = Array(edgeSize).fill(null).map -> Array(edgeSize)

  for row in [0...edgeSize]
    for col in [0...edgeSize]
      unless assembledTiles[row][col]
        for i, t of tiles
          unless usedIndices[i]
            for opt in allGridOrientations t.contents
              if row isnt 0
                currentTopRow = getRow opt, true
                bottomOfAbove = getRow assembledTiles[row-1][col].contents, false
                continue if currentTopRow isnt bottomOfAbove

              if col isnt 0
                currentLeftCol = getCol opt, true
                rightColOfLeft = getCol assembledTiles[row][col-1].contents, false
                continue if currentLeftCol isnt rightColOfLeft

              t.contents = opt
              assembledTiles[row][col] = t

              usedIndices[i] = true
              recurseResult = backtrackAssemble tiles, assembledTiles, usedIndices
              return recurseResult if recurseResult

              assembledTiles[row][col] = null
              usedIndices[i] = false

        return null unless assembledTiles[row][col]

  assembledTiles

getRow = (grid, firstRow) ->
  (if firstRow then grid[0] else grid[grid.length - 1]).join ''

getCol = (grid, firstCol) ->
  (if firstCol then (row[0] for row in grid) else (row[row.length - 1] for row in grid)).join ''

allGridOrientations = (grid) ->
  orientations = [grid]
  for i in [0...3]
    orientations.push rotateStringGrid orientations[orientations.length - 1]

  for i in [0...4]
    orientations.push mirrorStringGrid orientations[i]

  orientations

rotateStringGrid = (grid) ->
  rotated = Array(grid[0].length).fill(null).map -> Array(grid.length)
  for i in [0...grid.length]
    for j in [0...grid[0].length]
      rotated[grid[0].length - 1 - j][i] = grid[i][j]
  rotated

mirrorStringGrid = (grid) ->
  grid.map (row) -> row.slice().reverse()

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  result = solve data.trim()
  console.log result
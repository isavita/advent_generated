fs = require 'fs'

directions =
  e:  {q: 1, r: 0}
  se: {q: 0, r: 1}
  sw: {q: -1, r: 1}
  w:  {q: -1, r: 0}
  nw: {q: 0, r: -1}
  ne: {q: 1, r: -1}

getNeighbors = (tile) ->
  neighbors = []
  for dir of directions
    neighbors.push {q: tile.q + directions[dir].q, r: tile.r + directions[dir].r}
  neighbors

data = fs.readFileSync('input.txt', 'utf8')
lines = data.trim().split('\n')

blackTiles = {}

for line in lines
  coord = {q: 0, r: 0}
  i = 0
  while i < line.length
    switch line[i]
      when 'e', 'w'
        dir = line[i]
      when 'n', 's'
        dir = line[i..i+1]
        i++
    move = directions[dir]
    coord.q += move.q
    coord.r += move.r
    i++
  blackTiles[JSON.stringify(coord)] = not blackTiles[JSON.stringify(coord)]

for day in [0...100]
  tilesToCheck = {}
  for tile in Object.keys(blackTiles)
    if blackTiles[tile]
      tileObj = JSON.parse(tile)
      tilesToCheck[tile] = true
      for neighbor in getNeighbors(tileObj)
        tilesToCheck[JSON.stringify(neighbor)] = true

  newBlackTiles = {}
  for tile in Object.keys(tilesToCheck)
    blackNeighborCount = 0
    tileObj = JSON.parse(tile)
    for neighbor in getNeighbors(tileObj)
      if blackTiles[JSON.stringify(neighbor)]
        blackNeighborCount++
    if blackTiles[tile] and (blackNeighborCount == 1 or blackNeighborCount == 2)
      newBlackTiles[tile] = true
    else if not blackTiles[tile] and blackNeighborCount == 2
      newBlackTiles[tile] = true

  blackTiles = newBlackTiles

console.log Object.keys(blackTiles).length
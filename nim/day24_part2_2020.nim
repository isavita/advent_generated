import strutils, sequtils, tables

type Coordinate = object
  q, r: int

let directions = {
  "e": (1, 0),
  "se": (0, 1),
  "sw": (-1, 1),
  "w": (-1, 0),
  "nw": (0, -1),
  "ne": (1, -1)
}.toTable

proc `+`(a, b: Coordinate): Coordinate =
  result = Coordinate(q: a.q + b.q, r: a.r + b.r)

proc getNeighbors(tile: Coordinate): seq[Coordinate] =
  for dir in directions.values:
    result.add tile + Coordinate(q: dir[0], r: dir[1])

proc main =
  let file = readFile("input.txt")
  var blackTiles = initTable[Coordinate, bool]()
  for line in file.splitLines:
    var coord = Coordinate(q: 0, r: 0)
    var i = 0
    while i < line.len:
      var dir: string
      if line[i] in {'e', 'w'}:
        dir = $line[i]
      else:
        dir = line[i ..< i + 2]
        inc i
      let move = directions[dir]
      coord.q += move[0]
      coord.r += move[1]
      inc i
    blackTiles[coord] = not blackTiles.getOrDefault(coord)

  for day in 0 ..< 100:
    var tilesToCheck = initTable[Coordinate, bool]()
    for tile, _ in blackTiles:
      tilesToCheck[tile] = true
      for neighbor in getNeighbors(tile):
        tilesToCheck[neighbor] = true

    var newBlackTiles = initTable[Coordinate, bool]()
    for tile, _ in tilesToCheck:
      var blackNeighborCount = 0
      for neighbor in getNeighbors(tile):
        if blackTiles.getOrDefault(neighbor):
          inc blackNeighborCount
      if blackTiles.getOrDefault(tile) and (blackNeighborCount == 1 or blackNeighborCount == 2):
        newBlackTiles[tile] = true
      elif not blackTiles.getOrDefault(tile) and blackNeighborCount == 2:
        newBlackTiles[tile] = true

    blackTiles = newBlackTiles

  echo blackTiles.len

when isMainModule:
  main()
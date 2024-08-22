import strutils, sequtils, tables

type Coord = tuple[x, y: int]

proc flipTile(tile: Coord, blackTiles: var Table[Coord, bool]) =
  if tile in blackTiles:
    blackTiles.del(tile)
  else:
    blackTiles[tile] = true

proc countBlackTiles(filename: string): int =
  let file = readFile(filename)
  var blackTiles = initTable[Coord, bool]()
  for line in file.splitLines():
    var x, y: int
    var i = 0
    while i < line.len:
      case line[i]
      of 'e':
        x += 2
        inc(i)
      of 'w':
        x -= 2
        inc(i)
      of 'n':
        if line[i+1] == 'e':
          x += 1
          y += 1
        else:
          x -= 1
          y += 1
        inc(i, 2)
      of 's':
        if line[i+1] == 'e':
          x += 1
          y -= 1
        else:
          x -= 1
          y -= 1
        inc(i, 2)
      else:
        inc(i)
    flipTile((x, y), blackTiles)
  result = blackTiles.len

echo countBlackTiles("input.txt")
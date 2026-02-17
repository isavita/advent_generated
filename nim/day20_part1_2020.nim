
import strutils, tables, algorithm

proc getBorders(tile: seq[string]): seq[string] =
  result.add(tile[0])
  result.add(tile[tile.len - 1])
  var left = ""
  var right = ""
  for row in tile:
    if row.len > 0:
      left.add(row[0])
      right.add(row[row.len - 1])
  result.add(left)
  result.add(right)

proc main() =
  var tiles = initTable[int, seq[string]]()
  var currentTileId = -1

  try:
    let f = open("input.txt")
    defer: f.close()
    for line in f.lines:
      let trimmed = line.strip()
      if trimmed == "":
        continue
      if trimmed.startsWith("Tile "):
        currentTileId = parseInt(trimmed[5 .. ^2])
        tiles[currentTileId] = @[]
      elif currentTileId != -1:
        tiles[currentTileId].add(trimmed)
  except IOError:
    return

  var bordersMap = initTable[string, seq[int]]()
  for tileId, tileData in tiles:
    let currentBorders = getBorders(tileData)
    for b in currentBorders:
      var rb = b
      reverse(rb)
      bordersMap.mgetOrPut(b, @[]).add(tileId)
      bordersMap.mgetOrPut(rb, @[]).add(tileId)

  var product: int64 = 1
  for tileId, tileData in tiles:
    var uniqueBorderCount = 0
    let currentBorders = getBorders(tileData)
    for b in currentBorders:
      if bordersMap[b].len == 1:
        uniqueBorderCount += 1
    
    if uniqueBorderCount == 2:
      product *= tileId

  echo product

main()


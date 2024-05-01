import strutils, math, sequtils

type Coordinate = object
  x, y: int

proc parseCoordinates(input: string): seq[Coordinate] =
  let lines = input.splitLines()
  result = newSeq[Coordinate](lines.len)
  for i, line in lines:
    let parts = line.split(", ")
    result[i] = Coordinate(x: parseInt(parts[0]), y: parseInt(parts[1]))

proc findBoundingBox(coordinates: seq[Coordinate]): (int, int, int, int) =
  var minX, minY, maxX, maxY: int
  for c in coordinates:
    if c.x < minX or minX == 0: minX = c.x
    if c.y < minY or minY == 0: minY = c.y
    if c.x > maxX: maxX = c.x
    if c.y > maxY: maxY = c.y
  (minX, minY, maxX, maxY)

proc manhattanDistance(x1, y1, x2, y2: int): int =
  abs(x1 - x2) + abs(y1 - y2)

proc findRegionSize(coordinates: seq[Coordinate], maxDistance: int): int =
  let (minX, minY, maxX, maxY) = findBoundingBox(coordinates)
  var regionSize: int
  for x in minX..maxX:
    for y in minY..maxY:
      var totalDistance = 0
      for c in coordinates:
        totalDistance += manhattanDistance(x, y, c.x, c.y)
      if totalDistance < maxDistance:
        inc regionSize
  regionSize

when isMainModule:
  let input = readFile("input.txt")
  let coordinates = parseCoordinates(input)
  let regionSize = findRegionSize(coordinates, 10000)
  echo regionSize
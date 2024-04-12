fs = require 'fs'

manhattanDistance = (x1, y1, x2, y2) -> Math.abs(x1 - x2) + Math.abs(y1 - y2)

findBoundingBox = (coordinates) ->
  minX = minY = Infinity
  maxX = maxY = -Infinity
  for c in coordinates
    minX = Math.min(minX, c.x)
    minY = Math.min(minY, c.y)
    maxX = Math.max(maxX, c.x)
    maxY = Math.max(maxY, c.y)
  { minX, minY, maxX, maxY }

findRegionSize = (coordinates, maxDistance) ->
  { minX, minY, maxX, maxY } = findBoundingBox coordinates
  regionSize = 0
  for x in [minX..maxX]
    for y in [minY..maxY]
      totalDistance = 0
      for c in coordinates
        totalDistance += manhattanDistance x, y, c.x, c.y
      regionSize++ if totalDistance < maxDistance
  regionSize

parseCoordinates = (input) ->
  lines = input.trim().split '\n'
  coordinates = []
  for line in lines
    [x, y] = line.split(', ').map (item) -> parseInt item
    coordinates.push { x, y }
  coordinates

fs.readFile 'input.txt', 'utf8', (err, content) ->
  throw err if err
  coordinates = parseCoordinates content
  regionSize = findRegionSize coordinates, 10000
  console.log regionSize
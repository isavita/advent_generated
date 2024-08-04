fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').split('\n').map (line) -> line.split('').map Number

isLowPoint = (heightmap, x, y) ->
  height = heightmap[y][x]
  !(
    (x > 0 and heightmap[y][x-1] <= height) or
    (x < heightmap[y].length-1 and heightmap[y][x+1] <= height) or
    (y > 0 and heightmap[y-1][x] <= height) or
    (y < heightmap.length-1 and heightmap[y+1][x] <= height)
  )

exploreBasin = (heightmap, x, y, visited) ->
  return 0 if visited["#{x},#{y}"] or heightmap[y][x] == 9
  visited["#{x},#{y}"] = true
  size = 1

  directions = [[0, -1], [-1, 0], [0, 1], [1, 0]]
  for dir in directions
    [newX, newY] = [x + dir[0], y + dir[1]]
    if newX >= 0 and newX < heightmap[0].length and newY >= 0 and newY < heightmap.length
      size += exploreBasin(heightmap, newX, newY, visited)
  size

heightmap = input
basinSizes = []
visited = {}

for row, y in heightmap
  for x in [0...row.length]
    if isLowPoint(heightmap, x, y)
      size = exploreBasin(heightmap, x, y, visited)
      basinSizes.push size

basinSizes.sort (a, b) -> b - a
result = basinSizes[0] * basinSizes[1] * basinSizes[2]
console.log result
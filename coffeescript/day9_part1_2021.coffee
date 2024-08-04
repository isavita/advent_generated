fs = require 'fs'

isLowPoint = (heightmap, x, y) ->
  height = heightmap[y][x]
  return false if x > 0 and heightmap[y][x-1] <= height
  return false if x < heightmap[y].length - 1 and heightmap[y][x+1] <= height
  return false if y > 0 and heightmap[y-1][x] <= height
  return false if y < heightmap.length - 1 and heightmap[y+1][x] <= height
  true

input = fs.readFileSync('input.txt', 'utf8').split('\n')
heightmap = input.map (line) -> line.split('').map (char) -> parseInt(char, 10)

totalRiskLevel = 0
for row, y in heightmap
  for height, x in row
    totalRiskLevel += 1 + height if isLowPoint(heightmap, x, y)

console.log totalRiskLevel
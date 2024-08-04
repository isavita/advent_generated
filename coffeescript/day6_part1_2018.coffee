fs = require 'fs'

Point = (x, y) ->
  @X = x
  @Y = y

abs = (x) -> if x < 0 then -x else x

input = fs.readFileSync('input.txt', 'utf8').split('\n')
points = []
maxX = 0
maxY = 0

for line in input
  [x, y] = line.split(', ').map(Number)
  maxX = Math.max(maxX, x)
  maxY = Math.max(maxY, y)
  points.push(new Point(x, y))

grid = ([] for i in [0..maxX+1])
for row in grid
  row.push([] for j in [0..maxY+1])

areas = (0 for i in [0..points.length-1])
infinite = (false for i in [0..points.length-1])

for i in [0..maxX+1]
  for j in [0..maxY+1]
    minDist = maxX + maxY
    for k in [0..points.length-1]
      point = points[k]
      dist = abs(point.X - i) + abs(point.Y - j)
      if dist < minDist
        minDist = dist
        grid[i][j] = k
      else if dist == minDist
        grid[i][j] = -1

    if grid[i][j] != -1
      if i == 0 or j == 0 or i == maxX+1 or j == maxY+1
        infinite[grid[i][j]] = true
      areas[grid[i][j]]++

maxArea = 0
for area, i in areas
  if !infinite[i] and area > maxArea
    maxArea = area

console.log(maxArea)
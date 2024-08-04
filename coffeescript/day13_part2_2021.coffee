fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').split('\n')

points = {}
folds = []
readingPoints = true

for line in input
  if line is ''
    readingPoints = false
    continue
  if readingPoints
    [x, y] = line.split(',').map(Number)
    points["#{x},#{y}"] = true
  else
    [axis, val] = line.split('=')
    val = parseInt(val)
    if axis.includes('x')
      folds.push([val, 0])
    else
      folds.push([0, val])

for fold, i in folds
  newPoints = {}
  for point in Object.keys(points)
    [x, y] = point.split(',').map(Number)
    newPoint = [x, y]
    if fold[0] != 0 and x > fold[0]
      newPoint[0] = fold[0] - (x - fold[0])
    else if fold[1] != 0 and y > fold[1]
      newPoint[1] = fold[1] - (y - fold[1])
    newPoints["#{newPoint[0]},#{newPoint[1]}"] = true
  points = newPoints
  if i == 0
    console.log "Number of dots visible after first fold:", Object.keys(points).length

maxX = 0
maxY = 0
for point in Object.keys(points)
  [x, y] = point.split(',').map(Number)
  maxX = Math.max(maxX, x)
  maxY = Math.max(maxY, y)

grid = []
for i in [0..maxY]
  row = []
  for j in [0..maxX]
    row.push ' '
  grid.push row

for point in Object.keys(points)
  [x, y] = point.split(',').map(Number)
  grid[y][x] = '#'

for row in grid
  console.log row.join('')
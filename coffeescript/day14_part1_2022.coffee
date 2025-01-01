
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

grid = {}

for line in input
  parts = line.split(' -> ').map (p) ->
    [x, y] = p.split(',').map(Number)
    {x, y}
  for i in [0...parts.length - 1]
    p1 = parts[i]
    p2 = parts[i + 1]
    if p1.x == p2.x
      for y in [Math.min(p1.y, p2.y)..Math.max(p1.y, p2.y)]
        grid["#{p1.x},#{y}"] = true
    else
      for x in [Math.min(p1.x, p2.x)..Math.max(p1.x, p2.x)]
        grid["#{x},#{p1.y}"] = true

bounds = (grid) ->
  minX = Infinity
  maxX = -Infinity
  minY = Infinity
  maxY = -Infinity
  for key of grid
    [x, y] = key.split(',').map(Number)
    minX = Math.min(minX, x)
    maxX = Math.max(maxX, x)
    minY = Math.min(minY, y)
    maxY = Math.max(maxY, y)
  {minX, maxX, minY, maxY}

fill = (grid) ->
  floor = bounds(grid).maxY + 1
  sands = 0
  firstFloorTouch = 0
  while not grid["500,0"]
    sand = {x: 500, y: 0}
    settled = false
    while not settled
      nextPos = null
      for next in [{x: sand.x, y: sand.y + 1}, {x: sand.x - 1, y: sand.y + 1}, {x: sand.x + 1, y: sand.y + 1}]
        if not grid["#{next.x},#{next.y}"]
          nextPos = next
          break
      if nextPos
        sand = nextPos
        if sand.y == floor
          if firstFloorTouch == 0
            firstFloorTouch = sands
          grid["#{sand.x},#{sand.y}"] = true
          settled = true
      else
        grid["#{sand.x},#{sand.y}"] = true
        settled = true
    sands++
  firstFloorTouch

console.log fill(grid)

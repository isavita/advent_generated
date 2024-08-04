fs = require 'fs'

Point = (x, y) -> ({x, y})

getPoints = (path) ->
  points = {}
  current = Point(0, 0)
  moves = path.split(',')
  for move in moves
    dir = move[0]
    steps = parseInt(move.slice(1))
    for i in [0...steps]
      switch dir
        when 'U' then current.y++
        when 'D' then current.y--
        when 'L' then current.x--
        when 'R' then current.x++
      points["#{current.x},#{current.y}"] = true
  points

data = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
wire1 = getPoints(data[0])
wire2 = getPoints(data[1])

intersections = {}
for p in Object.keys(wire1)
  if wire2[p]
    intersections[p] = true

minDistance = Infinity
for p in Object.keys(intersections)
  [x, y] = p.split(',').map(Number)
  distance = Math.abs(x) + Math.abs(y)
  if distance < minDistance
    minDistance = distance

console.log(minDistance)
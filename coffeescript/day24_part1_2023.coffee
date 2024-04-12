fs = require 'fs'

class Coord
  constructor: (@x, @y, @z) ->

class Point
  constructor: (@pos, @vel) ->

parseInput = (input) ->
  points = []
  for line in input
    parts = line.split /,|@/
    pos = new Coord parseFloat(parts[0].trim()), parseFloat(parts[1].trim()), parseFloat(parts[2].trim())
    vel = new Coord parseFloat(parts[3].trim()), parseFloat(parts[4].trim()), parseFloat(parts[5].trim())
    points.push new Point(pos, vel)
  points

isIntersecting2D = (p1, p2) ->
  det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
  if det == 0
    return [false, new Coord(0,0,0), 0, 0]
  t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
  t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
  coord = new Coord(p1.pos.x + p1.vel.x * t1, p1.pos.y + p1.vel.y * t1, 0)
  [true, coord, t1, t2]

solve = (input, min, max) ->
  points = parseInput input
  cnt = 0
  for i in [0...points.length]
    for j in [0...i]
      [isIntersecting, coord, time1, time2] = isIntersecting2D(points[i], points[j])
      isInBound = min <= coord.x <= max and min <= coord.y <= max
      cnt++ if isIntersecting and isInBound and time1 >= 0 and time2 >= 0
  cnt

readFile = (fileName) ->
  fs.readFileSync(fileName, 'utf8').trim().split '\n'

main = ->
  input = readFile "input.txt"
  console.log solve(input, 200000000000000, 400000000000000)

main()
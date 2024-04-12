fs = require 'fs'

class Point
  constructor: (@x, @y, @z) ->

calculateExposedSides = (p, cubes) ->
  directions = [
    new Point(1, 0, 0), new Point(-1, 0, 0)
    new Point(0, 1, 0), new Point(0, -1, 0)
    new Point(0, 0, 1), new Point(0, 0, -1)
  ]
  exposedSides = 6
  for dir in directions
    adjacent = new Point(p.x + dir.x, p.y + dir.y, p.z + dir.z)
    adjacentKey = "#{adjacent.x},#{adjacent.y},#{adjacent.z}"
    if cubes[adjacentKey]
      exposedSides -= 1
  exposedSides

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  cubes = {}
  lines = data.trim().split '\n'
  for line in lines
    [x, y, z] = line.split(',').map (coord) -> parseInt(coord, 10)
    key = "#{x},#{y},#{z}"
    cubes[key] = true

  surfaceArea = 0
  for key, value of cubes
    [x, y, z] = key.split(',').map (coord) -> parseInt(coord, 10)
    p = new Point(x, y, z)
    surfaceArea += calculateExposedSides(p, cubes)

  console.log surfaceArea
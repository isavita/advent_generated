fs = require 'fs'

class Coord
  constructor: (@x, @y) ->

  add: (c) ->
    new Coord(@x + c.x, @y + c.y)

  multiplyByScalar: (s) ->
    new Coord(@x * s, @y * s)

North = new Coord(0, -1)
West = new Coord(-1, 0)
South = new Coord(0, 1)
East = new Coord(1, 0)

abs = (x) -> if x < 0 then -x else x

parseInput = (input) ->
  current = new Coord(0, 0)
  vertices = [current]
  for line in input
    [dirInput, lengthStr] = line.split ' '
    length = parseInt lengthStr, 10
    dir = switch dirInput.charAt(0)
      when 'U' then North
      when 'L' then West
      when 'D' then South
      when 'R' then East
    current = current.add dir.multiplyByScalar length
    vertices.push current
  vertices

shoelace = (vertices) ->
  n = vertices.length
  area = 0
  for i in [0...n]
    next = (i + 1) % n
    area += vertices[i].x * vertices[next].y
    area -= vertices[i].y * vertices[next].x
  abs(area) / 2

perimeter = (vertices) ->
  n = vertices.length
  perim = 0
  for i in [0...n]
    next = (i + 1) % n
    perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)
  perim

calculatePolygonArea = (vertices) ->
  shoelace(vertices) + perimeter(vertices) / 2 + 1

solve = (input) ->
  vertices = parseInput(input)
  calculatePolygonArea vertices

readFile = (fileName) ->
  fs.readFile fileName, 'utf8', (err, data) ->
    throw err if err
    input = data.trim().split '\n'
    console.log solve(input)

readFile 'input.txt'
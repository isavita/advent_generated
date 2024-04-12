fs = require 'fs'

class Coord
  constructor: (@X, @Y) ->

  add: (c2) ->
    new Coord(@X + c2.X, @Y + c2.Y)

  multiplyByScalar: (s) ->
    new Coord(@X * s, @Y * s)

North = new Coord(0, -1)
West = new Coord(-1, 0)
South = new Coord(0, 1)
East = new Coord(1, 0)

abs = (x) -> if x < 0 then -x else x

hexStringToInt = (hexStr) ->
  parseInt(hexStr, 16)

parseInput = (input) ->
  Up = '3'.charCodeAt(0)
  Left = '2'.charCodeAt(0)
  Down = '1'.charCodeAt(0)
  Right = '0'.charCodeAt(0)

  current = new Coord(0, 0)
  vertices = [current]

  for line in input
    parts = line.split " "
    color = parts[2]
    dirInput = color[7].charCodeAt(0)
    lengthStr = color[2..6]
    length = hexStringToInt(lengthStr)

    dir = switch dirInput
      when Up then North
      when Left then West
      when Down then South
      when Right then East

    current = current.add(dir.multiplyByScalar(length))
    vertices.push(current)

  vertices

shoelace = (vertices) ->
  n = vertices.length
  area = 0

  for i in [0...n]
    next = (i + 1) % n
    area += vertices[i].X * vertices[next].Y
    area -= vertices[i].Y * vertices[next].X

  abs(area) / 2

perimeter = (vertices) ->
  n = vertices.length
  perim = 0

  for i in [0...n]
    next = (i + 1) % n
    perim += abs(vertices[i].X - vertices[next].X) + abs(vertices[i].Y - vertices[next].Y)

  perim

calculatePolygonArea = (vertices) ->
  shoelace(vertices) + Math.floor(perimeter(vertices) / 2) + 1

solve = (input) ->
  vertices = parseInput(input)
  calculatePolygonArea(vertices)

readFile = (fileName) ->
  fs.readFileSync(fileName, 'utf8').trim().split("\n")

input = readFile "input.txt"
console.log solve(input)
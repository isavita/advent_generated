fs = require 'fs'

maxInt = (a, b) -> if a > b then a else b
minInt = (a, b) -> -maxInt(-a, -b)

class Cube
  constructor: (@isOn, @x1, @x2, @y1, @y2, @z1, @z2) ->

  getIntersection: (c2) ->
    x1 = maxInt @x1, c2.x1
    x2 = minInt @x2, c2.x2
    y1 = maxInt @y1, c2.y1
    y2 = minInt @y2, c2.y2
    z1 = maxInt @z1, c2.z1
    z2 = minInt @z2, c2.z2

    return [null, false] if x1 > x2 or y1 > y2 or z1 > z2

    intersectionState = if @isOn and c2.isOn then false else if not @isOn and not c2.isOn then true else c2.isOn

    [new Cube(intersectionState, x1, x2, y1, y2, z1, z2), true]

  volume: ->
    vol = (@x2 - @x1 + 1) * (@y2 - @y1 + 1) * (@z2 - @z1 + 1)
    return vol if @isOn
    -vol

parseInput = (input) ->
  ans = []
  for line in input.trim().split "\n"
    parts = line.split " "
    [x1, x2, y1, y2, z1, z2] = parts[1].match(/-?\d+/g).map(Number)
    ans.push new Cube parts[0] == "on", x1, x2, y1, y2, z1, z2
  ans

solve = (input) ->
  cubes = parseInput input
  finalList = []

  for c in cubes
    toAdd = []
    for finalCube in finalList
      [intersection, didIntersect] = finalCube.getIntersection c
      toAdd.push intersection if didIntersect

    toAdd.push c if c.isOn

    finalList.push toAdd...

  total = 0
  total += c.volume() for c in finalList
  total

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  result = solve data
  console.log result
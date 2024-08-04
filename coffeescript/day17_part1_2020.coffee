fs = require 'fs'

class Coordinate
  constructor: (@x, @y, @z) ->

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
activeCubes = {}

for line, y in input
  for char, x in line
    if char is '#'
      activeCubes["#{x},#{y},0"] = true

simulateCycle = (activeCubes) ->
  newActiveCubes = {}
  neighborCounts = {}

  for coord of activeCubes
    [x, y, z] = coord.split(',').map(Number)
    for dz in [-1..1]
      for dy in [-1..1]
        for dx in [-1..1]
          if dz is 0 and dy is 0 and dx is 0
            continue
          neighbor = new Coordinate(x + dx, y + dy, z + dz)
          neighborKey = "#{neighbor.x},#{neighbor.y},#{neighbor.z}"
          neighborCounts[neighborKey] ?= 0
          neighborCounts[neighborKey]++

  for coord, count of neighborCounts
    if count is 3 or (count is 2 and activeCubes[coord])
      newActiveCubes[coord] = true

  newActiveCubes

for cycle in [0..5]
  activeCubes = simulateCycle(activeCubes)

console.log Object.keys(activeCubes).length
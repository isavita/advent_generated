fs = require 'fs'

class Nanobot
  constructor: (@x, @y, @z, @radius) ->

parseNanobots = (data) ->
  nanobots = []
  regex = /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/g
  while match = regex.exec(data)
    [x, y, z, radius] = match.slice(1).map(Number)
    nanobots.push new Nanobot(x, y, z, radius)
  nanobots

findStrongestNanobot = (nanobots) ->
  strongest = nanobots[0]
  for nanobot in nanobots
    if nanobot.radius > strongest.radius
      strongest = nanobot
  strongest

countNanobotsInRange = (nanobots, strongest) ->
  count = 0
  for nanobot in nanobots
    if manhattanDistance(nanobot, strongest) <= strongest.radius
      count += 1
  count

manhattanDistance = (a, b) ->
  Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z)

data = fs.readFileSync('input.txt', 'utf8')
nanobots = parseNanobots(data)
strongest = findStrongestNanobot(nanobots)
inRangeCount = countNanobotsInRange(nanobots, strongest)

console.log inRangeCount
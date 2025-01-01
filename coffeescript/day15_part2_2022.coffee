
fs = require 'fs'

manhattan = (p, q) -> Math.abs(p.x - q.x) + Math.abs(p.y - q.y)

distress = (sensors, maxcoord) ->
  for x in [0..maxcoord]
    y = 0
    while y <= maxcoord
      detected = false
      skip = 0
      for s in sensors
        if manhattan({x: x, y: y}, s.pos) <= s.dist
          detected = true
          dist = s.dist - Math.abs(s.pos.x - x)
          skip = Math.max(skip, dist + s.pos.y - y)
      if not detected
        return x * 4000000 + y
      y += skip + 1
  return -1

input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
sensors = []
for line in input
  match = line.match(/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/)
  s = {
    pos: { x: parseInt(match[1]), y: parseInt(match[2]) }
    beacon: { x: parseInt(match[3]), y: parseInt(match[4]) }
  }
  s.dist = manhattan(s.pos, s.beacon)
  sensors.push s

console.log distress(sensors, 4000000)

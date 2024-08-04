fs = require 'fs'

class Asteroid
  constructor: (@x, @y, @angle, @dist) ->

readAsteroids = (filename) ->
  data = fs.readFileSync(filename, 'utf8')
  data.split('\n').map (line) ->
    line.split('').map (char) -> char is '#'

vaporizeAsteroids = (asteroids, station) ->
  targets = []
  for y in [0...asteroids.length]
    for x in [0...asteroids[0].length]
      if asteroids[y][x] and not (x is station[0] and y is station[1])
        angle = Math.atan2(y - station[1], x - station[0])
        dist = Math.hypot(x - station[0], y - station[1])
        if angle < -Math.PI / 2
          angle += 2 * Math.PI
        targets.push new Asteroid(x, y, angle, dist)

  targets.sort (a, b) ->
    if a.angle is b.angle
      a.dist - b.dist
    else
      a.angle - b.angle

  vaporized = []
  while targets.length > 0
    lastAngle = -Number.MAX_VALUE
    i = 0
    while i < targets.length
      if targets[i].angle isnt lastAngle
        vaporized.push targets[i]
        lastAngle = targets[i].angle
        targets.splice i, 1
      else
        i++
  vaporized

findBestAsteroidLocation = (asteroids) ->
  maxCount = 0
  bestLocation = null
  for y in [0...asteroids.length]
    for x in [0...asteroids[0].length]
      if asteroids[y][x]
        count = countVisibleAsteroids(asteroids, x, y)
        if count > maxCount
          maxCount = count
          bestLocation = [x, y]
  [bestLocation, maxCount]

countVisibleAsteroids = (asteroids, x, y) ->
  angles = {}
  for otherY in [0...asteroids.length]
    for otherX in [0...asteroids[0].length]
      if asteroids[otherY][otherX] and not (otherX is x and otherY is y)
        angle = Math.atan2(otherY - y, otherX - x)
        angles[angle] = true
  Object.keys(angles).length

asteroids = readAsteroids 'input.txt'
[station, _] = findBestAsteroidLocation asteroids
vaporized = vaporizeAsteroids asteroids, station
if vaporized.length >= 200
  result = vaporized[199].x * 100 + vaporized[199].y
  console.log result
else
  console.log 'Less than 200 asteroids were vaporized.'
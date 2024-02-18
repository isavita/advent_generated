
fs = require 'fs'

readAsteroids = (filename) ->
  asteroids = []
  contents = fs.readFileSync(filename, 'utf8')
  lines = contents.split('\n')
  for y in [0...lines.length]
    row = lines[y].split('')
    asteroids.push row.map (c) -> c == '#'
  return asteroids

findBestAsteroidLocation = (asteroids) ->
  maxCount = 0
  for y in [0...asteroids.length]
    for x in [0...asteroids[y].length]
      if asteroids[y][x]
        count = countVisibleAsteroids(asteroids, x, y)
        maxCount = count if count > maxCount
  return maxCount

countVisibleAsteroids = (asteroids, x, y) ->
  angles = {}
  for otherY in [0...asteroids.length]
    for otherX in [0...asteroids[otherY].length]
      if asteroids[otherY][otherX] and not (otherX == x and otherY == y)
        dx = otherX - x
        dy = otherY - y
        angle = Math.atan2(dy, dx)
        angles[angle] = true
  return Object.keys(angles).length

asteroids = readAsteroids("input.txt")
maxCount = findBestAsteroidLocation(asteroids)
console.log(maxCount)

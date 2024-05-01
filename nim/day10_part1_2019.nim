import math
import sequtils
import strutils

proc readAsteroids(filename: string): seq[seq[bool]] =
  var file = readFile(filename)
  var asteroids: seq[seq[bool]]
  for line in file.splitLines:
    var asteroidRow: seq[bool]
    for char in line:
      asteroidRow.add(char == '#')
    asteroids.add(asteroidRow)
  asteroids

proc countVisibleAsteroids(asteroids: seq[seq[bool]], x, y: int): int =
  var angles: seq[float64]
  for otherY, row in asteroids:
    for otherX, isAsteroid in row:
      if isAsteroid and (otherX != x or otherY != y):
        let angle = arctan2(float64(otherY-y), float64(otherX-x))
        if not angles.anyIt(it == angle):
          angles.add(angle)
  angles.len

proc findBestAsteroidLocation(asteroids: seq[seq[bool]]): int =
  var maxCount = 0
  var bestLocation = (-1, -1)
  for y, row in asteroids:
    for x, isAsteroid in row:
      if isAsteroid:
        let count = countVisibleAsteroids(asteroids, x, y)
        if count > maxCount:
          maxCount = count
          bestLocation = (x, y)
  echo "Best location: ", bestLocation
  maxCount

let asteroids = readAsteroids("input.txt")
echo findBestAsteroidLocation(asteroids)
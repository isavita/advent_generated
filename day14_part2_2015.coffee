
fs = require('fs')

class Reindeer
  constructor: (@speed, @flyTime, @restTime, @distance, @points, @flying, @timeInMode) ->

readReindeerDetails = (filename) ->
  reindeers = []
  contents = fs.readFileSync(filename, 'utf8').split('\n')
  for line in contents
    parts = line.split(' ')
    speed = parseInt(parts[3])
    flyTime = parseInt(parts[6])
    restTime = parseInt(parts[13])
    reindeers.push new Reindeer(speed, flyTime, restTime, 0, 0, true, 0)
  return reindeers

simulateRaceWithPoints = (reindeers, totalSeconds) ->
  for i in [0...totalSeconds]
    maxDistance = 0
    for j in [0...reindeers.length]
      reindeer = reindeers[j]
      if reindeer.flying
        reindeer.distance += reindeer.speed
      reindeer.timeInMode++
      if (reindeer.flying and reindeer.timeInMode == reindeer.flyTime) or (not reindeer.flying and reindeer.timeInMode == reindeer.restTime)
        reindeer.flying = not reindeer.flying
        reindeer.timeInMode = 0
      if reindeer.distance > maxDistance
        maxDistance = reindeer.distance
    for j in [0...reindeers.length]
      reindeer = reindeers[j]
      if reindeer.distance == maxDistance
        reindeer.points++

findMaxPoints = (reindeers) ->
  maxPoints = 0
  for reindeer in reindeers
    if reindeer.points > maxPoints
      maxPoints = reindeer.points
  return maxPoints

reindeers = readReindeerDetails('input.txt')
simulateRaceWithPoints(reindeers, 2503)
maxPoints = findMaxPoints(reindeers)
console.log maxPoints

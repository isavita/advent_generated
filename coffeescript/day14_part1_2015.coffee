fs = require 'fs'

class Reindeer
  constructor: (@speed, @flyTime, @restTime) ->
    @distance = 0
    @flying = true
    @timeInMode = 0

simulateRace = (reindeers, totalSeconds) ->
  for i in [0...totalSeconds]
    for reindeer in reindeers
      if reindeer.flying
        reindeer.distance += reindeer.speed
        reindeer.timeInMode++
        if reindeer.timeInMode == reindeer.flyTime
          reindeer.flying = false
          reindeer.timeInMode = 0
      else
        reindeer.timeInMode++
        if reindeer.timeInMode == reindeer.restTime
          reindeer.flying = true
          reindeer.timeInMode = 0

findMaxDistance = (reindeers) ->
  maxDistance = 0
  for reindeer in reindeers
    if reindeer.distance > maxDistance
      maxDistance = reindeer.distance
  maxDistance

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading input:", err
    return

  reindeers = []
  lines = data.trim().split '\n'
  for line in lines
    parts = line.split ' '
    speed = parseInt parts[3]
    flyTime = parseInt parts[6]
    restTime = parseInt parts[13]
    reindeers.push new Reindeer speed, flyTime, restTime

  simulateRace reindeers, 2503
  maxDistance = findMaxDistance reindeers
  console.log maxDistance
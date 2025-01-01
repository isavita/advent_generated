
fs = require 'fs'

manhattanDistance = (x1, y1, x2, y2) ->
  Math.abs(x1 - x2) + Math.abs(y1 - y2)

parseInput = (input) ->
  lines = input.trim().split('\n')
  sensors = []
  for line in lines
    match = line.match(/Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/)
    if match
      sensorX = parseInt(match[1])
      sensorY = parseInt(match[2])
      beaconX = parseInt(match[3])
      beaconY = parseInt(match[4])
      sensors.push({sensorX, sensorY, beaconX, beaconY})
  sensors

solve = (sensors, targetY) ->
  coveredRanges = []
  for sensor in sensors
    distance = manhattanDistance(sensor.sensorX, sensor.sensorY, sensor.beaconX, sensor.beaconY)
    yDiff = Math.abs(sensor.sensorY - targetY)
    if yDiff <= distance
      xDiff = distance - yDiff
      coveredRanges.push({start: sensor.sensorX - xDiff, end: sensor.sensorX + xDiff})

  coveredRanges.sort((a, b) -> a.start - b.start)

  mergedRanges = []
  for range in coveredRanges
    if mergedRanges.length == 0
      mergedRanges.push(range)
    else
      lastRange = mergedRanges[mergedRanges.length - 1]
      if range.start <= lastRange.end + 1
        lastRange.end = Math.max(lastRange.end, range.end)
      else
        mergedRanges.push(range)

  totalCovered = 0
  for range in mergedRanges
    totalCovered += (range.end - range.start + 1)

  # Remove beacons that are on the target row
  beaconsOnTargetRow = new Set()
  for sensor in sensors
    if sensor.beaconY == targetY
      beaconsOnTargetRow.add(sensor.beaconX)

  totalCovered -= beaconsOnTargetRow.size
  totalCovered

input = fs.readFileSync('input.txt', 'utf8')
sensors = parseInput(input)
targetY = 2000000
result = solve(sensors, targetY)
console.log result

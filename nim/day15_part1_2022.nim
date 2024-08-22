import strutils, sequtils, tables

type
  Point = tuple[x, y: int]
  Sensor = tuple[sensor, beacon: Point]

proc parseInput(filename: string): seq[Sensor] =
  result = @[]
  for line in lines(filename):
    let parts = line.split(": closest beacon is at ")
    let sensorCoords = parts[0].split("Sensor at ")[1].split(", ")
    let beaconCoords = parts[1].split(", ")
    let sensor = (x: parseInt(sensorCoords[0].split("=")[1]), y: parseInt(sensorCoords[1].split("=")[1]))
    let beacon = (x: parseInt(beaconCoords[0].split("=")[1]), y: parseInt(beaconCoords[1].split("=")[1]))
    result.add((sensor: sensor, beacon: beacon))

proc manhattanDistance(p1, p2: Point): int =
  abs(p1.x - p2.x) + abs(p1.y - p2.y)

proc countExcludedPositions(sensors: seq[Sensor], targetY: int): int =
  var excluded = initTable[int, bool]()
  for sensor in sensors:
    let distance = manhattanDistance(sensor.sensor, sensor.beacon)
    if abs(sensor.sensor.y - targetY) <= distance:
      let xRange = distance - abs(sensor.sensor.y - targetY)
      for x in sensor.sensor.x - xRange .. sensor.sensor.x + xRange:
        if (x, targetY) != sensor.beacon:
          excluded[x] = true
  result = excluded.len

let sensors = parseInput("input.txt")
echo countExcludedPositions(sensors, 2000000)
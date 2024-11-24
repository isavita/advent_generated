
import std/[strutils, math, sequtils]

type
  Sensor = object
    px, py: int
    bx, by: int
    dist: int

proc manhattan(x1, y1, x2, y2: int): int =
  abs(x1 - x2) + abs(y1 - y2)

proc distress(sensors: seq[Sensor], maxcoord: int): int =
  for x in 0..maxcoord:
    var y = 0
    while y <= maxcoord:
      var detected = false
      var skip = 0
      
      for s in sensors:
        let manhattanDist = manhattan(s.px, s.py, x, y)
        if manhattanDist <= s.dist:
          detected = true
          let vertDist = s.dist - abs(s.px - x)
          skip = max(skip, vertDist + s.py - y)
      
      if not detected:
        return x * 4_000_000 + y
      
      y += max(1, skip)

proc main() =
  let input = readFile("input.txt")
  var sensors: seq[Sensor]
  
  for line in input.strip().splitLines():
    var s: Sensor
    let parts = line.split(": ")
    let sensorPart = parts[0].split("x=")[1].split(", y=")
    let beaconPart = parts[1].split("x=")[1].split(", y=")
    
    s.px = parseInt(sensorPart[0])
    s.py = parseInt(sensorPart[1])
    s.bx = parseInt(beaconPart[0])
    s.by = parseInt(beaconPart[1])
    s.dist = manhattan(s.px, s.py, s.bx, s.by)
    
    sensors.add(s)
  
  echo distress(sensors, 4_000_000)

main()

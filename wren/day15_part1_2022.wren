
import "io" for File

var lines = File.read("input.txt").trim().split("\n")
var sensors = []
for (line in lines) {
  if (line.isEmpty) continue
  var parts = line.split(": closest beacon is at ")
  var sensorPart = parts[0].replace("Sensor at ", "")
  var beaconPart = parts[1]
  var sensorCoords = sensorPart.split(", ")
  var beaconCoords = beaconPart.split(", ")
  var sx = Num.fromString(sensorCoords[0].split("=")[1])
  var sy = Num.fromString(sensorCoords[1].split("=")[1])
  var bx = Num.fromString(beaconCoords[0].split("=")[1])
  var by = Num.fromString(beaconCoords[1].split("=")[1])
  sensors.add([[sx, sy], [bx, by]])
}

var targetY = 2000000
var intervals = []
var beaconsOnRow = {}

for (sensor in sensors) {
  var s = sensor[0]
  var b = sensor[1]
  var sx = s[0]
  var sy = s[1]
  var bx = b[0]
  var by = b[1]
  var d = (sx - bx).abs + (sy - by).abs
  var dy = (sy - targetY).abs
  if (dy <= d) {
    var left = sx - (d - dy)
    var right = sx + (d - dy)
    intervals.add([left, right])
  }
  if (by == targetY) {
    beaconsOnRow[bx] = true
  }
}

if (intervals.isEmpty) {
  System.print(0)
} else {
  intervals.sort {|a,b| a[0] < b[0] }
  var merged = []
  for (interval in intervals) {
    if (merged.isEmpty) {
      merged.add(interval)
    } else {
      var last = merged[-1]
      if (interval[0] <= last[1] + 1) {
        last[1] = (last[1] > interval[1]) ? last[1] : interval[1]
      } else {
        merged.add(interval)
      }
    }
  }
  var total = 0
  for (m in merged) {
    total = total + (m[1] - m[0] + 1)
  }
  var beaconCount = 0
  for (x in beaconsOnRow) { beaconCount = beaconCount + 1 }
  System.print(total - beaconCount)
}

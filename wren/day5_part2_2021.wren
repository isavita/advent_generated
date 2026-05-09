
import "io" for File

var overlaps = {}
var content = File.read("input.txt")
var lines = content.split("\n")
for (lineStr in lines) {
  var line = lineStr.trim()
  if (line.count == 0) continue
  var parts = line.split(" -> ")
  var startParts = parts[0].split(",")
  var endParts = parts[1].split(",")
  var x1 = Num.fromString(startParts[0])
  var y1 = Num.fromString(startParts[1])
  var x2 = Num.fromString(endParts[0])
  var y2 = Num.fromString(endParts[1])
  var dx = x2 - x1
  var dy = y2 - y1
  var xStep = dx > 0 ? 1 : dx < 0 ? -1 : 0
  var yStep = dy > 0 ? 1 : dy < 0 ? -1 : 0
  var steps = dx.abs > dy.abs ? dx.abs : dy.abs
  var currX = x1
  var currY = y1
  for (i in 0..steps) {
    var point = "%(currX),%(currY)"
    var count = overlaps[point]
    if (count == null) count = 0
    overlaps[point] = count + 1
    currX = currX + xStep
    currY = currY + yStep
  }
}
var countOverlaps = 0
for (value in overlaps.values) {
  if (value > 1) countOverlaps = countOverlaps + 1
}
System.print(countOverlaps)

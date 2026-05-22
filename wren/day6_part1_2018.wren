
import "io" for File

var content = File.read("input.txt")
var points = []
var maxX = 0
var maxY = 0

for (rawLine in content.split("\n")) {
    var line = rawLine.trim()
    if (line != "") {
        var parts = line.split(", ")
        var x = Num.fromString(parts[0])
        var y = Num.fromString(parts[1])
        if (x > maxX) maxX = x
        if (y > maxY) maxY = y
        points.add([x, y])
    }
}

var n = points.count
var areas = List.filled(n, 0)
var infinite = List.filled(n, false)

for (x in 0..(maxX + 1)) {
    for (y in 0..(maxY + 1)) {
        var minDist = 999999
        var closestIdx = -1
        var tie = false
        for (k in 0...n) {
            var p = points[k]
            var dx = p[0] - x
            if (dx < 0) dx = -dx
            var dy = p[1] - y
            if (dy < 0) dy = -dy
            var dist = dx + dy
            if (dist < minDist) {
                minDist = dist
                closestIdx = k
                tie = false
            } else if (dist == minDist) {
                tie = true
            }
        }
        if (!tie) {
            if (x == 0 || y == 0 || x == maxX + 1 || y == maxY + 1) {
                infinite[closestIdx] = true
            }
            areas[closestIdx] = areas[closestIdx] + 1
        }
    }
}

var maxArea = 0
for (i in 0...n) {
    if (!infinite[i] && areas[i] > maxArea) {
        maxArea = areas[i]
    }
}
System.print(maxArea)

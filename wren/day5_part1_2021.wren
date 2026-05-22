
import "io" for File

var lines = File.read("input.txt").split("\n")
var grid = {}
for (line in lines) {
    var trimmed = line.trim()
    if (trimmed != "") {
        var parts = trimmed.split(" -> ")
        var start = parts[0].split(",")
        var end = parts[1].split(",")
        var x1 = Num.fromString(start[0])
        var y1 = Num.fromString(start[1])
        var x2 = Num.fromString(end[0])
        var y2 = Num.fromString(end[1])
        if (x1 == x2) {
            var min = y1 < y2 ? y1 : y2
            var max = y1 > y2 ? y1 : y2
            for (y in min..max) {
                var key = "%(x1),%(y)"
                grid[key] = (grid[key] || 0) + 1
            }
        } else if (y1 == y2) {
            var min = x1 < x2 ? x1 : x2
            var max = x1 > x2 ? x1 : x2
            for (x in min..max) {
                var key = "%(x),%(y1)"
                grid[key] = (grid[key] || 0) + 1
            }
        }
    }
}
var overlaps = 0
for (v in grid.values) {
    if (v > 1) overlaps = overlaps + 1
}
System.print(overlaps)

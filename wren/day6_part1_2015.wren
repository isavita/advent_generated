
import "io" for File

var grid = List.filled(1000000, false)
var lines = File.read("input.txt").trim().split("\n")

for (line in lines) {
    var action = 0
    var data = ""
    if (line.startsWith("toggle")) {
        action = 3
        data = line[7..-1]
    } else if (line.startsWith("turn on")) {
        action = 1
        data = line[8..-1]
    } else {
        action = 2
        data = line[9..-1]
    }

    var parts = data.split(" through ")
    var start = parts[0].split(",")
    var end = parts[1].split(",")

    var x1 = Num.fromString(start[0])
    var y1 = Num.fromString(start[1])
    var x2 = Num.fromString(end[0])
    var y2 = Num.fromString(end[1])

    for (x in x1..x2) {
        var offset = x * 1000
        for (y in y1..y2) {
            var i = offset + y
            if (action == 1) {
                grid[i] = true
            } else if (action == 2) {
                grid[i] = false
            } else {
                grid[i] = !grid[i]
            }
        }
    }
}

var count = 0
for (light in grid) if (light) count = count + 1
System.print("Lights that are on: %(count)")

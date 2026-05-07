
import "io" for File

var grid = List.filled(1000000, 0)
var lines = File.read("input.txt").trim().split("\n")

for (line in lines) {
    var action = 0
    var coords = ""
    if (line.startsWith("toggle ")) {
        action = 2
        coords = line[7..-1]
    } else if (line.startsWith("turn on ")) {
        action = 1
        coords = line[8..-1]
    } else if (line.startsWith("turn off ")) {
        action = -1
        coords = line[9..-1]
    }

    var parts = coords.split(" through ")
    var start = parts[0].split(",")
    var endP = parts[1].split(",")

    var x1 = Num.fromString(start[0])
    var y1 = Num.fromString(start[1])
    var x2 = Num.fromString(endP[0])
    var y2 = Num.fromString(endP[1])

    for (x in x1..x2) {
        var row = x * 1000
        for (y in y1..y2) {
            var i = row + y
            if (action == 1) {
                grid[i] = grid[i] + 1
            } else if (action == 2) {
                grid[i] = grid[i] + 2
            } else {
                grid[i] = (grid[i] > 0) ? grid[i] - 1 : 0
            }
        }
    }
}

var total = 0
for (val in grid) total = total + val
System.print(total)

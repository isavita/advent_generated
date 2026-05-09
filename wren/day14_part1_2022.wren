import "io" for File

var grid = {}
var maxY = 0

var content = File.read("input.txt")
var lines = content.split("\n")
for (line in lines) {
    line = line.trim()
    if (line == "") continue
    var segments = line.split(" -> ")
    var points = []
    for (seg in segments) {
        var parts = seg.split(",")
        var x = Num.fromString(parts[0])
        var y = Num.fromString(parts[1])
        points.add([x, y])
    }
    for (i in 0...points.count-1) {
        var p1 = points[i]
        var p2 = points[i+1]
        if (p1[0] == p2[0]) {
            var startY = p1[1]
            var endY = p2[1]
            if (startY > endY) {
                startY = p2[1]
                endY = p1[1]
            }
            for (y in startY..endY) {
                var key = "%(p1[0]),%(y)"
                grid[key] = true
                if (y > maxY) maxY = y
            }
        } else {
            var startX = p1[0]
            var endX = p2[0]
            if (startX > endX) {
                startX = p2[0]
                endX = p1[0]
            }
            for (x in startX..endX) {
                var key = "%(x),%(p1[1])"
                grid[key] = true
                if (p1[1] > maxY) maxY = p1[1]
            }
        }
    }
}

var floorY = maxY + 1
var sands = 0
var firstFloorTouch = 0

while (true) {
    var sand = [500, 0]
    var resting = false
    while (!resting) {
        if (sand[1] == floorY - 1) {
            if (firstFloorTouch == 0) firstFloorTouch = sands
            grid["%(sand[0]),%(sand[1])"] = true
            resting = true
            break
        }
        var moved = false
        var deltas = [[0,1], [-1,1], [1,1]]
        for (delta in deltas) {
            var newX = sand[0] + delta[0]
            var newY = sand[1] + delta[1]
            var key = "%(newX),%(newY)"
            if (!grid.containsKey(key)) {
                sand = [newX, newY]
                moved = true
                break
            }
        }
        if (moved) continue
        grid["%(sand[0]),%(sand[1])"] = true
        resting = true
    }
    sands = sands + 1
    if (grid.containsKey("500,0")) {
        if (firstFloorTouch == 0) firstFloorTouch = sands
        break
    }
}

System.print(firstFloorTouch)

import "io" for File

var content = File.read("input.txt")
var X = [0]
var Y = [0]
var x = 0
var y = 0

for (line in content.split("\n")) {
    var parts = []
    var current = ""
    for (i in 0...line.count) {
        var char = line[i]
        if (char == " " || char == "\t" || char == "\r" || char == "\n") {
            if (current != "") {
                parts.add(current)
                current = ""
            }
        } else {
            current = current + char
        }
    }
    if (current != "") parts.add(current)

    if (parts.isEmpty) continue

    var dir = parts[0]
    var len = Num.fromString(parts[1])

    var dx = 0
    var dy = 0
    if (dir == "U") {
        dy = -1
    } else if (dir == "D") {
        dy = 1
    } else if (dir == "L") {
        dx = -1
    } else if (dir == "R") {
        dx = 1
    } else {
        continue
    }

    x = x + dx * len
    y = y + dy * len
    X.add(x)
    Y.add(y)
}

var n = X.count
var area2 = 0
for (i in 0...n) {
    var nxt = (i + 1) % n
    area2 = area2 + X[i] * Y[nxt] - Y[i] * X[nxt]
}
if (area2 < 0) area2 = -area2
var shoelace = (area2 / 2).floor

var per = 0
for (i in 0...n) {
    var nxt = (i + 1) % n
    var dx = (X[i] - X[nxt]).abs
    var dy = (Y[i] - Y[nxt]).abs
    per = per + dx + dy
}
var per2 = (per / 2).floor

var ans = shoelace + per2 + 1
System.print(ans)

import "io" for File

var content = File.read("input.txt")
content = content.trim()
var lines = []
var temp = ""
for (b in content.bytes) {
    if (b == 10) {
        lines.add(temp)
        temp = ""
    } else {
        temp = temp + String.fromByte(b)
    }
}
lines.add(temp)

var instructions = lines[0]
var desertMap = {}
for (i in 2...lines.count) {
    var line = lines[i]
    if (line.count >= 16) {
        var key = line[0..2]
        var left = line[7..9]
        var right = line[12..14]
        desertMap[key] = [left, right]
    }
}

var current = "AAA"
var steps = 0
var instructionsLen = instructions.count
while (current != "ZZZ") {
    var idx = steps % instructionsLen
    var dir = instructions[idx..idx]
    if (dir == "L") {
        current = desertMap[current][0]
    } else if (dir == "R") {
        current = desertMap[current][1]
    }
    steps = steps + 1
}
System.print(steps)
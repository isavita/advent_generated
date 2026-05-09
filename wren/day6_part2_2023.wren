
import "io" for File

var content = File.read("input.txt")
if (content == null) {
    System.print("Error opening file")
    return
}

var lines = content.split("\n")
var time = 0
var distance = 0
var lineNumber = 0
for (line in lines) {
    if (lineNumber == 0) {
        var parts = line.split(":")
        var value = parts[1].replace(" ", "")
        time = Num.fromString(value)
    } else if (lineNumber == 1) {
        var parts = line.split(":")
        var value = parts[1].replace(" ", "")
        distance = Num.fromString(value)
    }
    lineNumber = lineNumber + 1
    if (lineNumber == 2) break
}

var t = time
var r = distance
var D = t*t - 4*r
if (D < 0) {
    System.print(0)
} else {
    var sqrtD = D.sqrt
    var root1 = (t - sqrtD) / 2
    var root2 = (t + sqrtD) / 2
    var minHold = root1.floor + 1
    var maxHold = root2.ceil - 1
    if (minHold < 1) minHold = 1
    if (maxHold > t-1) maxHold = t-1
    if (minHold > maxHold) {
        System.print(0)
    } else {
        System.print(maxHold - minHold + 1)
    }
}

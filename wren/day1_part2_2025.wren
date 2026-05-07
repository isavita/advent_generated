
import "io" for File

var floorDiv = Fn.new { |a, b| (a / b).floor }

var content = File.read("input.txt")
if (content == null) {
    System.print("Error reading file")
    return
}

var lines = content.split("\n")
var pos = 50
var hits = 0

for (line in lines) {
    if (line.count == 0) continue
    var dir = line[0]
    var amt = 0
    if (line.count > 1) {
        var amtStr = line[1..line.count-1]
        amt = Num.fromString(amtStr) || 0
    }
    if (dir == "R") {
        var newPos = pos + amt
        hits = hits + (newPos / 100).floor
        pos = newPos % 100
    } else {
        hits = hits + floorDiv.call(pos - 1, 100) - floorDiv.call(pos - amt - 1, 100)
        pos = (pos - amt) % 100
        if (pos < 0) pos = pos + 100
    }
}

System.print(hits)

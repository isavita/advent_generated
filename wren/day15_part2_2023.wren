
import "io" for File

var hash = Fn.new { |s|
    var h = 0
    for (c in s.bytes) {
        h = ((h + c) * 17) % 256
    }
    return h
}

var content = File.read("input.txt").trim().replace("\n", "").replace("\r", "")
var steps = content.split(",")

var boxes = List.filled(256, null)
for (i in 0...256) {
    boxes[i] = []
}

for (step in steps) {
    if (step.endsWith("-")) {
        var label = step[0...-1]
        var b = hash.call(label)
        var box = boxes[b]
        var found = -1
        for (i in 0...box.count) {
            if (box[i][0] == label) {
                found = i
                break
            }
        }
        if (found != -1) {
            box.removeAt(found)
        }
    } else {
        var parts = step.split("=")
        var label = parts[0]
        var value = Num.fromString(parts[1])
        var b = hash.call(label)
        var box = boxes[b]
        var found = -1
        for (i in 0...box.count) {
            if (box[i][0] == label) {
                found = i
                break
            }
        }
        if (found != -1) {
            box[found][1] = value
        } else {
            box.add([label, value])
        }
    }
}

var tot = 0
for (b in 0...256) {
    var box = boxes[b]
    for (i in 0...box.count) {
        tot = tot + (b + 1) * (i + 1) * box[i][1]
    }
}
System.print(tot)

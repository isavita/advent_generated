
import "io" for File

var lines = File.read("input.txt").split("\n")
// remove possible trailing empty line
if (lines[-1] == "") lines = lines[0..-2]

var height = lines.count
var width  = lines[0].count

// locate the start position 'S'
var sx = 0
var sy = 0
for (y in 0...height) {
    var row = lines[y]
    for (x in 0...width) {
        if (row[x] == "S") {
            sx = x
            sy = y
            break
        }
    }
}

// active[x] == true  → a path can reach column x on the current row
var active = List.filled(width, false)
active[sx] = true
var splits = 0

for (y in sy...height) {
    var next = List.filled(width, false)
    for (x in 0...width) {
        if (!active[x]) continue
        var ch = lines[y][x]
        if (ch == "^") {
            splits = splits + 1
            if (x > 0)       next[x - 1] = true
            if (x < width-1) next[x + 1] = true
        } else {
            next[x] = true
        }
    }
    active = next
    if (!active.any {|v| v}) break
}

System.print(splits)

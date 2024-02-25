
import strutils

var file = open("input.txt")
var lines = file.readAll.splitLines

var count = 0
for line in lines:
    var parts = line.split(" | ")
    var output = parts[1].split(" ")
    for digit in output:
        case len(digit)
        of 2, 4, 3, 7:
            count.inc
        else:
            discard

echo count

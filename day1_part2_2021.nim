
import strutils

var file = open("input.txt")
var lines = file.readAll.splitLines
file.close()

var vals: seq[int] = @[]
for line in lines:
    if line != "":
        vals.add(parseInt(line))

var prevSum = vals[0] + vals[1] + vals[2]
var count = 0
for i in 3..<vals.len:
    var currSum = vals[i-2] + vals[i-1] + vals[i]
    if currSum > prevSum:
        count.inc()
    prevSum = currSum

echo count

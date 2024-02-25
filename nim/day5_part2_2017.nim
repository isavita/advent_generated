
import os
import strutils

var file = open("input.txt")
var lines = file.readAll().splitLines()

var offsets: seq[int] = @[]

for line in lines:
    offsets.add(parseInt(line))

var index = 0
var steps = 0

while index >= 0 and index < offsets.len:
    var jump = offsets[index]

    if jump >= 3:
        offsets[index] -= 1
    else:
        offsets[index] += 1

    index += jump
    steps += 1

echo steps

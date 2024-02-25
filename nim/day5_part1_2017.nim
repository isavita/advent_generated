
import os
import strutils

var offsets: seq[int]

let data = readFile("input.txt")
for line in data.splitLines():
  offsets.add(parseInt(line))

var index = 0
var steps = 0

while index >= 0 and index < offsets.len:
  let jump = offsets[index]
  offsets[index] += 1
  index += jump
  inc(steps)

echo steps

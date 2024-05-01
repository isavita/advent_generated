import strutils, sequtils

let data = readFile("input.txt")
let steps = parseInt(strip(data))
var buffer = @[0]
var currentPos = 0

for i in 1..2017:
  currentPos = (currentPos + steps) mod buffer.len
  buffer.insert(i, currentPos + 1)
  inc currentPos

for i, val in buffer.pairs:
  if val == 2017:
    echo buffer[(i + 1) mod buffer.len]
    break
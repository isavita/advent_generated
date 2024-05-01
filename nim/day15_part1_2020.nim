import sequtils, strutils, tables

let input = readFile("input.txt").strip().split(",").map(parseInt)

var spoken = initTable[int, int]()
var lastSpoken = 0
var turn = 1

for num in input:
  spoken[num] = turn
  lastSpoken = num
  turn += 1

while turn <= 2020:
  if lastSpoken notin spoken:
    spoken[lastSpoken] = turn - 1
    lastSpoken = 0
  else:
    let prevTurn = spoken[lastSpoken]
    spoken[lastSpoken] = turn - 1
    lastSpoken = turn - 1 - prevTurn
  turn += 1

echo lastSpoken
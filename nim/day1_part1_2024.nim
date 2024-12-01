import std/[strutils, algorithm, sequtils]

let content = readFile("input.txt")
var leftList, rightList: seq[int]

for line in content.splitLines():
  if line.len == 0: continue
  let numbers = line.splitWhitespace()
  if numbers.len >= 2:
    leftList.add(parseInt(numbers[0]))
    rightList.add(parseInt(numbers[^1]))

leftList.sort()
rightList.sort()

var totalDistance = 0
for i in 0..<leftList.len:
  totalDistance += abs(leftList[i] - rightList[i])

echo "Total distance: ", totalDistance

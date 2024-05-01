import sequtils, strutils, tables

let input = readFile("input.txt").splitLines().mapIt(it.parseInt())

var frequency = 0
for change in input:
  frequency += change
echo "Part 1: The resulting frequency is ", frequency

var seen = initTable[int, bool]()
var frequency2 = 0
var found = false
while not found:
  for change in input:
    frequency2 += change
    if seen.hasKey(frequency2):
      echo "Part 2: The first frequency reached twice is ", frequency2
      found = true
      break
    seen[frequency2] = true
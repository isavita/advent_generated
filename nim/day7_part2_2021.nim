import strutils, sequtils, math, algorithm

let file = open("input.txt")
var positions: seq[int]
for line in file.lines:
  for num in line.split(","):
    positions.add parseInt(num)

sort(positions)  # Use the sort procedure from the algorithm module

var minFuel = high(int)
for i in positions[0]..positions[^1]:
  var fuel = 0
  for pos in positions:
    let diff = abs(pos - i)
    fuel += diff * (diff + 1) div 2
  if fuel < minFuel:
    minFuel = fuel

echo minFuel
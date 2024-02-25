
import math, strutils

var file = open("input.txt")
let target = parseInt(file.readLine())

var sideLength = ceil(sqrt(float(target))).int
if sideLength mod 2 == 0:
  sideLength += 1

var maxValue = sideLength * sideLength
var stepsFromEdge = (sideLength - 1) div 2
var distanceToMiddle = 0

for i in 0..3:
  var middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i
  var distance = abs(target - middlePoint)
  if distance < distanceToMiddle or i == 0:
    distanceToMiddle = distance

var manhattanDistance = stepsFromEdge + distanceToMiddle

echo manhattanDistance

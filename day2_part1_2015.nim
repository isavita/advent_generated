
import strutils

var file = open("input.txt")
var lines = file.readAll().splitLines()
file.close()

var total = 0
for line in lines:
  var dimensions = line.split("x")
  if dimensions.len != 3:
    quit("Invalid input format")

  var l = parseInt(dimensions[0])
  var w = parseInt(dimensions[1])
  var h = parseInt(dimensions[2])

  var side1 = l * w
  var side2 = w * h
  var side3 = h * l

  var smallest = min(min(side1, side2), side3)
  total += 2*side1 + 2*side2 + 2*side3 + smallest

echo total

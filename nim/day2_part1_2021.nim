
import os
import strutils

var file = open("input.txt")
var lines = file.readAll().splitLines

var horizontalPosition = 0
var depth = 0

for line in lines:
  var command = line.split(" ")
  var direction = command[0]
  var units = parseInt(command[1])

  case direction
  of "forward":
    horizontalPosition += units
  of "down":
    depth += units
  of "up":
    depth -= units

var product = horizontalPosition * depth
echo product

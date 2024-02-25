
import os
import strutils

var file = open("input.txt")
var lines = file.readAll().splitLines

var horizontalPosition = 0
var depth = 0
var aim = 0

for line in lines:
  var command = line.split(" ")
  var direction = command[0]
  var units = parseInt(command[1])

  case direction
  of "forward":
    horizontalPosition += units
    depth += aim * units
  of "down":
    aim += units
  of "up":
    aim -= units

var product = horizontalPosition * depth
echo product

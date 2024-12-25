
import "io" for File

var instructions = File.read("input.txt").split("\n").where { |line| line != "" }.toList

var x = 0
var y = 0
var direction = 0 // 0: East, 1: South, 2: West, 3: North

for (instruction in instructions) {
  var action = instruction[0]
  var value = Num.fromString(instruction[1..-1])

  if (action == "N") {
    y = y + value
  } else if (action == "S") {
    y = y - value
  } else if (action == "E") {
    x = x + value
  } else if (action == "W") {
    x = x - value
  } else if (action == "L") {
    direction = (direction - (value / 90) % 4 + 4) % 4
  } else if (action == "R") {
    direction = (direction + (value / 90) % 4) % 4
  } else if (action == "F") {
    if (direction == 0) {
      x = x + value
    } else if (direction == 1) {
      y = y - value
    } else if (direction == 2) {
      x = x - value
    } else if (direction == 3) {
      y = y + value
    }
  }
}

var manhattanDistance = x.abs + y.abs
System.print(manhattanDistance)

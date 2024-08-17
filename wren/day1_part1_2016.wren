import "io" for File

class Position {
  construct new(x, y, dirIndex) {
    _x = x
    _y = y
    _dirIndex = dirIndex
  }

  x { _x }
  x=(value) { _x = value }

  y { _y }
  y=(value) { _y = value }

  dirIndex { _dirIndex }
  dirIndex=(value) { _dirIndex = value }
}

var abs = Fn.new { |x| x < 0 ? -x : x }

var main = Fn.new {
  var input = File.read("input.txt")
  var instructions = input.split(", ")

  var pos = Position.new(0, 0, 0)
  var directions = [
    [0, 1],
    [1, 0],
    [0, -1],
    [-1, 0]
  ]

  for (instruction in instructions) {
    var turn = instruction[0]
    var blocks = Num.fromString(instruction[1..-1])

    if (turn == "R") {
      pos.dirIndex = (pos.dirIndex + 1) % 4
    } else {
      pos.dirIndex = (pos.dirIndex - 1 + 4) % 4
    }

    pos.x = pos.x + directions[pos.dirIndex][0] * blocks
    pos.y = pos.y + directions[pos.dirIndex][1] * blocks
  }

  System.print(abs.call(pos.x) + abs.call(pos.y))
}

main.call()
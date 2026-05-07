import "io" for File

var parsePath = Fn.new { |line|
  var moves = []
  var parts = line.split(",")
  for (part in parts) {
    var dir = part[0..0]
    var distStr = part[1..-1]
    var dist = Num.fromString(distStr)
    moves.add([dir, dist])
  }
  return moves
}

var input = File.read("input.txt").trim()
var lines = input.split("\n")
var wire1Line = lines[0]
var wire2Line = lines[1]

var wire1Moves = parsePath.call(wire1Line)
var wire2Moves = parsePath.call(wire2Line)

var wire1Points = {}

var x = 0
var y = 0
for (move in wire1Moves) {
  var dir = move[0]
  var dist = move[1]
  for (i in 1..dist) {
    if (dir == "U") {
      y = y + 1
    } else if (dir == "D") {
      y = y - 1
    } else if (dir == "L") {
      x = x - 1
    } else if (dir == "R") {
      x = x + 1
    }
    var key = "%(x),%(y)"
    wire1Points[key] = true
  }
}

var x2 = 0
var y2 = 0
var minDist = null
for (move in wire2Moves) {
  var dir = move[0]
  var dist = move[1]
  for (i in 1..dist) {
    if (dir == "U") {
      y2 = y2 + 1
    } else if (dir == "D") {
      y2 = y2 - 1
    } else if (dir == "L") {
      x2 = x2 - 1
    } else if (dir == "R") {
      x2 = x2 + 1
    }
    var key = "%(x2),%(y2)"
    if (wire1Points[key]) {
      var manhattan = x2.abs + y2.abs
      if (minDist == null || manhattan < minDist) {
        minDist = manhattan
      }
    }
  }
}

System.print("Minimum distance: %(minDist)")
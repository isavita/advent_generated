
import "io" for File

var input = File.read("input.txt")
var lines = []
for (line in input.split("\n")) {
  var trimmed = line.trim()
  if (trimmed.count > 0) {
    lines.add(trimmed)
  }
}

var height = lines.count
var width = lines[0].count
var grid = List.filled(width * height, 0)

for (y in 0...height) {
  var line = lines[y]
  for (x in 0...width) {
    var char = line[x]
    if (char == ">") {
      grid[y * width + x] = 1
    } else if (char == "v") {
      grid[y * width + x] = 2
    } else {
      grid[y * width + x] = 0
    }
  }
}

var steps = 0
while (true) {
  var moved = false
  
  var eastMoves = []
  for (y in 0...height) {
    var rowOffset = y * width
    for (x in 0...width) {
      var idx = rowOffset + x
      if (grid[idx] == 1) {
        var nextX = (x + 1 == width) ? 0 : x + 1
        var nextIdx = rowOffset + nextX
        if (grid[nextIdx] == 0) {
          eastMoves.add(idx)
          eastMoves.add(nextIdx)
        }
      }
    }
  }
  if (eastMoves.count > 0) {
    moved = true
    var i = 0
    while (i < eastMoves.count) {
      grid[eastMoves[i]] = 0
      grid[eastMoves[i+1]] = 1
      i = i + 2
    }
  }

  var southMoves = []
  for (y in 0...height) {
    var rowOffset = y * width
    var nextY = (y + 1 == height) ? 0 : y + 1
    var nextRowOffset = nextY * width
    for (x in 0...width) {
      var idx = rowOffset + x
      if (grid[idx] == 2) {
        var nextIdx = nextRowOffset + x
        if (grid[nextIdx] == 0) {
          southMoves.add(idx)
          southMoves.add(nextIdx)
        }
      }
    }
  }
  if (southMoves.count > 0) {
    moved = true
    var i = 0
    while (i < southMoves.count) {
      grid[southMoves[i]] = 0
      grid[southMoves[i+1]] = 2
      i = i + 2
    }
  }

  steps = steps + 1
  if (!moved) break
}

System.print(steps)

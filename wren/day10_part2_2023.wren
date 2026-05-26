
import "io" for File

var tileToPipe = {
  "|": "TB",
  "-": "RL",
  "J": "TL",
  "L": "TR",
  "7": "BL",
  "F": "BR"
}

var pipeToTile = {
  "TR": "L",
  "TB": "|",
  "TL": "J",
  "RB": "F",
  "RL": "-",
  "BL": "7"
}

var dirs = ["T", "R", "B", "L"]
var dx = { "T": 0, "R": 1, "B": 0, "L": -1 }
var dy = { "T": -1, "R": 0, "B": 1, "L": 0 }
var opposite = { "T": "B", "B": "T", "R": "L", "L": "R" }

var content = File.read("input.txt")
var lines = content.replace("\r", "").split("\n")
if (lines.count > 0 && lines[-1] == "") {
  lines.removeAt(-1)
}
var max_y = lines.count
var max_x = lines[0].count

var startX = -1
var startY = -1
for (y in 0...max_y) {
  for (x in 0...max_x) {
    if (lines[y][x] == "S") {
      startX = x
      startY = y
      break
    }
  }
  if (startX != -1) break
}

var startPipe = ""
for (d in dirs) {
  var nx = startX + dx[d]
  var ny = startY + dy[d]
  if (nx >= 0 && nx < max_x && ny >= 0 && ny < max_y) {
    var nt = lines[ny][nx]
    var np = tileToPipe[nt] || ""
    if (np.contains(opposite[d])) {
      startPipe = startPipe + d
    }
  }
}

var pathGrid = List.filled(max_x * max_y, ".")
var dir = startPipe[0]
var prev = dir
var cx = startX + dx[dir]
var cy = startY + dy[dir]

pathGrid[startY * max_x + startX] = pipeToTile[startPipe]

while (cx != startX || cy != startY) {
  var idx = cy * max_x + cx
  var tile = lines[cy][cx]
  pathGrid[idx] = tile
  var currentPipe = tileToPipe[tile] || ""
  for (i in 0...currentPipe.count) {
    var nd = currentPipe[i]
    if (nd != opposite[prev]) {
      prev = nd
      cx = cx + dx[nd]
      cy = cy + dy[nd]
      break
    }
  }
}

var isInside = Fn.new {|cx, cy|
  var idx = cy * max_x + cx
  if (pathGrid[idx] != ".") return false
  var startPipeChar = "."
  var numLeft = 0
  for (xx in 0...cx) {
    var v = pathGrid[cy * max_x + xx]
    if (v != ".") {
      if (v == "|") {
        numLeft = numLeft + 1
      } else if (v == "L") {
        startPipeChar = "L"
      } else if (v == "F") {
        startPipeChar = "F"
      } else if (v == "J") {
        if (startPipeChar == "F") {
          startPipeChar = "."
          numLeft = numLeft + 1
        } else if (startPipeChar == "L") {
          startPipeChar = "."
        }
      } else if (v == "7") {
        if (startPipeChar == "L") {
          startPipeChar = "."
          numLeft = numLeft + 1
        } else if (startPipeChar == "F") {
          startPipeChar = "."
        }
      }
    }
  }
  return (numLeft % 2) == 1
}

var count = 0
for (y in 0...max_y) {
  for (x in 0...max_x) {
    if (isInside.call(x, y)) {
      count = count + 1
    }
  }
}
System.print(count)

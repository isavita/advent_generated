
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")

var x = List.filled(10, 0)
var y = List.filled(10, 0)

var visited = {}
visited["0,0"] = true

for (line in lines) {
  if (line == "") continue
  var parts = line.split(" ")
  var dir = parts[0]
  var steps = Num.fromString(parts[1])
  
  for (s in 0...steps) {
    if (dir == "R") {
      x[0] = x[0] + 1
    } else if (dir == "L") {
      x[0] = x[0] - 1
    } else if (dir == "U") {
      y[0] = y[0] + 1
    } else if (dir == "D") {
      y[0] = y[0] - 1
    }
    
    for (i in 1..9) {
      var dx = x[i-1] - x[i]
      var dy = y[i-1] - y[i]
      if (dx.abs > 1 || dy.abs > 1) {
        if (dx > 0) {
          x[i] = x[i] + 1
        } else if (dx < 0) {
          x[i] = x[i] - 1
        }
        if (dy > 0) {
          y[i] = y[i] + 1
        } else if (dy < 0) {
          y[i] = y[i] - 1
        }
      }
    }
    visited["%(x[9]),%(y[9])"] = true
  }
}

System.print(visited.count)

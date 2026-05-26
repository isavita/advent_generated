
import "io" for File

var lines = File.read("input.txt").split("\n")
var hx = 0
var hy = 0
var tx = 0
var ty = 0
var visited = { "0,0": true }

var sign = Fn.new { |x| x > 0 ? 1 : (x < 0 ? -1 : 0) }

for (line in lines) {
  if (line == "") continue
  var parts = line.split(" ")
  var dir = parts[0]
  var steps = Num.fromString(parts[1])

  for (i in 0...steps) {
    if (dir == "R") hx = hx + 1
    if (dir == "L") hx = hx - 1
    if (dir == "U") hy = hy + 1
    if (dir == "D") hy = hy - 1

    var dx = hx - tx
    var dy = hy - ty
    if (dx.abs > 1 || dy.abs > 1) {
      tx = tx + sign.call(dx)
      ty = ty + sign.call(dy)
    }
    visited["%(tx),%(ty)"] = true
  }
}

System.print(visited.count)

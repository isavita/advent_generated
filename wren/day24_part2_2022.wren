
import "io" for File

var content = File.read("input.txt").trim().replace("\r", "")
var lines = content.split("\n")
var H = lines.count - 2
var W = lines[0].count - 2

var SX = 0
for (i in 0...lines[0].count) {
  if (lines[0][i] == ".") {
    SX = i - 1
    break
  }
}
var SY = -1

var GX = 0
var lastLine = lines[lines.count - 1]
for (i in 0...lastLine.count) {
  if (lastLine[i] == ".") {
    GX = i - 1
    break
  }
}
var GY = H

var g = List.filled(W * H, 0)
for (y in 0...H) {
  var line = lines[y + 1]
  for (x in 0...W) {
    var char = line[x + 1]
    var val = 0
    if (char == ">") val = 1
    if (char == "<") val = 2
    if (char == "v") val = 4
    if (char == "^") val = 8
    g[y * W + x] = val
  }
}

var gcd = Fn.new {|a, b|
  while (b != 0) {
    var t = a % b
    a = b
    b = t
  }
  return a
}

var lcm = Fn.new {|a, b| ((a * b) / gcd.call(a, b)).floor }
var P = lcm.call(W, H)

var isSafe = Fn.new {|x, y, t|
  if ((x == SX && y == SY) || (x == GX && y == GY)) return true
  if (x < 0 || x >= W || y < 0 || y >= H) return false
  var tw = t % W
  var th = t % H
  var xl = (x - tw + W) % W
  var xr = (x + tw) % W
  var yu = (y - th + H) % H
  var yd = (y + th) % H
  return !(g[y * W + xl] == 1 || g[y * W + xr] == 2 || g[yu * W + x] == 4 || g[yd * W + x] == 8)
}

var dx = [0, 1, -1, 0, 0]
var dy = [0, 0, 0, 1, -1]

var solve = Fn.new {|startX, startY, targetX, targetY, startTime|
  var q = [startX, startY, startTime]
  var qp = 0
  var visited = {}
  visited[((startY + 1) * W + startX) * P + (startTime % P)] = true

  while (qp < q.count) {
    var cx = q[qp]
    var cy = q[qp+1]
    var ct = q[qp+2]
    qp = qp + 3

    var nt = ct + 1
    var mt = nt % P

    for (i in 0...5) {
      var nx = cx + dx[i]
      var ny = cy + dy[i]

      if (nx == targetX && ny == targetY) {
        return nt
      }

      if (isSafe.call(nx, ny, nt)) {
        var state = ((ny + 1) * W + nx) * P + mt
        if (!visited.containsKey(state)) {
          visited[state] = true
          q.add(nx)
          q.add(ny)
          q.add(nt)
        }
      }
    }
  }
  return -1
}

var t = solve.call(SX, SY, GX, GY, 0)
t = solve.call(GX, GY, SX, SY, t)
System.print(solve.call(SX, SY, GX, GY, t))

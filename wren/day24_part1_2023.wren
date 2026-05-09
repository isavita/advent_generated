
import "io" for File

class Coord {
  construct new(x, y, z) {
    _x = x
    _y = y
    _z = z
  }
  x { _x }
  y { _y }
  z { _z }
}

class Point {
  construct new(pos, vel) {
    _pos = pos
    _vel = vel
  }
  pos { _pos }
  vel { _vel }
}

var epsilon = 1e-9

var parseInput = Fn.new { |lines|
  var points = []
  for (line in lines) {
    if (line == "") continue
    var parts = line.split("@")
    var posParts = parts[0].trim().split(",")
    var velParts = parts[1].trim().split(",")
    var pos = Coord.new(
      Num.fromString(posParts[0].trim()),
      Num.fromString(posParts[1].trim()),
      Num.fromString(posParts[2].trim())
    )
    var vel = Coord.new(
      Num.fromString(velParts[0].trim()),
      Num.fromString(velParts[1].trim()),
      Num.fromString(velParts[2].trim())
    )
    points.add(Point.new(pos, vel))
  }
  return points
}

var intersects2D = Fn.new { |p1, p2|
  var det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
  if (det.abs < epsilon) return null
  var t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
  var t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
  var ix = p1.pos.x + p1.vel.x * t1
  var iy = p1.pos.y + p1.vel.y * t1
  return [ix, iy, t1, t2]
}

var minCoord = 200000000000000.0
var maxCoord = 400000000000000.0

var content = File.read("input.txt").trim()
var lines = content.split("\n")
var points = parseInput.call(lines)
var count = 0
for (i in 0...points.count) {
  for (j in 0...i) {
    var res = intersects2D.call(points[i], points[j])
    if (res) {
      var ix = res[0]
      var iy = res[1]
      var t1 = res[2]
      var t2 = res[3]
      if (ix >= minCoord && ix <= maxCoord && iy >= minCoord && iy <= maxCoord && t1 >= 0 && t2 >= 0) {
        count = count + 1
      }
    }
  }
}
System.print(count)

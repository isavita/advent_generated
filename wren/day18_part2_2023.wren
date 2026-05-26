
import "io" for File

var hexToDec = Fn.new { |s|
  var v = 0
  for (c in s.codePoints) {
    var val = 0
    if (c >= 48 && c <= 57) {
      val = c - 48
    } else if (c >= 97 && c <= 102) {
      val = c - 97 + 10
    } else if (c >= 65 && c <= 70) {
      val = c - 65 + 10
    }
    v = v * 16 + val
  }
  return v
}

var content = File.read("input.txt")
var lines = content.split("\n")

var x = 0
var y = 0
var area2 = 0
var per = 0

for (line in lines) {
  if (line.isEmpty) continue
  var p = line.indexOf("(")
  if (p == -1) continue
  var q = line.indexOf(")")
  if (q <= p) continue
  if (q - p - 2 < 6) continue
  var color = line[p + 2...q]
  if (color.count < 6) continue
  var lenstr = color[0...5]
  var dir = color[5]
  var len = hexToDec.call(lenstr)
  
  var dx = 0
  var dy = 0
  if (dir == "0") {
    dx = len
  } else if (dir == "1") {
    dy = len
  } else if (dir == "2") {
    dx = -len
  } else if (dir == "3") {
    dy = -len
  }
  
  var nx = x + dx
  var ny = y + dy
  area2 = area2 + x * ny - y * nx
  x = nx
  y = ny
  per = per + len
}

if (area2 < 0) area2 = -area2
var area = (area2 / 2).floor
var total = area + (per / 2).floor + 1
System.print(total)

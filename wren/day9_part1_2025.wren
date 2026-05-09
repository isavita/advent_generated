import "io" for File

var xs = []
var ys = []

var content = File.read("input.txt")
var lines = content.split("\n")
for (line in lines) {
  var trimmed = line.trim()
  if (trimmed == "") continue
  var parts = trimmed.split(",")
  if (parts.count != 2) continue
  var xStr = parts[0].trim()
  var yStr = parts[1].trim()
  var x = Num.fromString(xStr)
  var y = Num.fromString(yStr)
  if (x == null || y == null) continue
  xs.add(x)
  ys.add(y)
}

var n = xs.count
if (n > 0) {
  var maxArea = 1
  for (i in 0...n) {
    var x1 = xs[i]
    var y1 = ys[i]
    for (j in (i+1)...n) {
      var x2 = xs[j]
      var y2 = ys[j]
      var minX = x1 < x2 ? x1 : x2
      var maxX = x1 > x2 ? x1 : x2
      var minY = y1 < y2 ? y1 : y2
      var maxY = y1 > y2 ? y1 : y2
      var width = maxX - minX + 1
      var height = maxY - minY + 1
      var area = width * height
      if (area > maxArea) maxArea = area
    }
  }
  System.print(maxArea)
}
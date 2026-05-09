
import "io" for File

var content = File.read("input.txt")
var lines = content.split("\n")
var points = []
for (line in lines) {
  line = line.trim()
  if (line == "") continue
  var parts = line.split(",")
  if (parts.count == 3) {
    var x = Num.fromString(parts[0].trim())
    var y = Num.fromString(parts[1].trim())
    var z = Num.fromString(parts[2].trim())
    points.add([x, y, z])
  }
}
var n = points.count
if (n < 2) {
  System.print("0")
} else {
  var edges = []
  for (i in 0...n) {
    for (j in i+1...n) {
      var p1 = points[i]
      var p2 = points[j]
      var dx = p1[0] - p2[0]
      var dy = p1[1] - p2[1]
      var dz = p1[2] - p2[2]
      var dist = dx*dx + dy*dy + dz*dz
      edges.add([i, j, dist])
    }
  }
  edges.sort {|a,b| a[2] < b[2] }
  var parent = (0...n).map {|i| i }.toList
  var size = (0...n).map {|i| 1 }.toList
  var find = Fn.new {|x|
    while (parent[x] != x) {
      parent[x] = parent[parent[x]]
      x = parent[x]
    }
    return x
  }
  var union = Fn.new {|a,b|
    var ra = find.call(a)
    var rb = find.call(b)
    if (ra == rb) return
    if (size[ra] < size[rb]) {
      parent[ra] = rb
      size[rb] = size[rb] + size[ra]
    } else {
      parent[rb] = ra
      size[ra] = size[ra] + size[rb]
    }
  }
  var limit = edges.count < 1000 ? edges.count : 1000
  for (i in 0...limit) {
    union.call(edges[i][0], edges[i][1])
  }
  var top = [0,0,0]
  for (i in 0...n) {
    if (parent[i] == i) {
      var s = size[i]
      if (s > top[0]) {
        top[2] = top[1]
        top[1] = top[0]
        top[0] = s
      } else if (s > top[1]) {
        top[2] = top[1]
        top[1] = s
      } else if (s > top[2]) {
        top[2] = s
      }
    }
  }
  var prod = 1
  for (i in 0..2) {
    if (top[i] > 0) prod = prod * top[i]
  }
  System.print(prod)
}

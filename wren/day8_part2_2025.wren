
import "io" for File

var content = File.read("input.txt")
if (content == null) Fiber.abort("Failed to read input.txt")
var lines = content.split("\n")
var pts = []
for (line in lines) {
  line = line.trim()
  if (line == "") continue
  var parts = line.split(",")
  if (parts.count == 3) {
    var x = Num.fromString(parts[0].trim())
    var y = Num.fromString(parts[1].trim())
    var z = Num.fromString(parts[2].trim())
    pts.add([x, y, z])
  }
}

if (pts.count < 2) return

var edges = []
for (i in 0...pts.count) {
  for (j in i+1...pts.count) {
    var a = pts[i]
    var b = pts[j]
    var dx = a[0] - b[0]
    var dy = a[1] - b[1]
    var dz = a[2] - b[2]
    var d = dx*dx + dy*dy + dz*dz
    edges.add([i, j, d])
  }
}

edges.sort {|a, b| a[2] < b[2]}

var parent = []
var rank = []
for (i in 0...pts.count) {
  parent.add(i)
  rank.add(0)
}

var find = Fn.new {|x|
  while (parent[x] != x) {
    parent[x] = parent[parent[x]]
    x = parent[x]
  }
  return x
}

var comps = pts.count
for (e in edges) {
  var u = e[0]
  var v = e[1]
  var ru = find.call(u)
  var rv = find.call(v)
  if (ru != rv) {
    if (rank[ru] < rank[rv]) {
      parent[ru] = rv
    } else if (rank[ru] > rank[rv]) {
      parent[rv] = ru
    } else {
      parent[rv] = ru
      rank[ru] = rank[ru] + 1
    }
    comps = comps - 1
    if (comps == 1) {
      var p1 = pts[u]
      var p2 = pts[v]
      System.print("Connected %(p1[0]),%(p1[1]),%(p1[2]) and %(p2[0]),%(p2[1]),%(p2[2])")
      System.print("Product of X coordinates: %(p1[0] * p2[0])")
      break
    }
  }
}

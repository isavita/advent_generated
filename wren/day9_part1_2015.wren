
import "io" for File

var lines = File.read("input.txt").trim().split("\n")
var dists = {}
var cities = {}

for (line in lines) {
  var p = line.trim().split(" ")
  if (p.count == 5) {
    var c1 = p[0]
    var c2 = p[2]
    var d = Num.fromString(p[4])
    dists[c1 + c2] = d
    dists[c2 + c1] = d
    cities[c1] = true
    cities[c2] = true
  }
}

var list = cities.keys.toList
var min = 1/0

var solve
solve = Fn.new {|k|
  if (k == list.count) {
    var d = 0
    for (i in 0...list.count - 1) {
      d = d + dists[list[i] + list[i+1]]
    }
    if (d < min) min = d
  } else {
    for (i in k...list.count) {
      var t = list[k]
      list[k] = list[i]
      list[i] = t
      solve.call(k + 1)
      t = list[k]
      list[k] = list[i]
      list[i] = t
    }
  }
}

if (list.count == 0) {
  System.print(0)
} else {
  solve.call(0)
  System.print(min)
}

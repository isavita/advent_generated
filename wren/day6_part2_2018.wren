
import "io" for File

var coords = File.read("input.txt").split("\n").where { |l| l.trim() != "" }.map { |l|
  var p = l.split(",")
  return [Num.fromString(p[0].trim()), Num.fromString(p[1].trim())]
}.toList

var regionSize = 0
for (i in 0..500) {
  for (j in 0..500) {
    var dist = 0
    for (c in coords) {
      dist = dist + (i - c[0]).abs + (j - c[1]).abs
    }
    if (dist < 10000) regionSize = regionSize + 1
  }
}

System.print(regionSize)

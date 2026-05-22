
import "io" for File

var seeds = []
var maps = []
var currentMap = null

for (line in File.read("input.txt").split("\n")) {
  line = line.trim()
  if (line.startsWith("seeds:")) {
    for (part in line.split(" ")) {
      var n = Num.fromString(part)
      if (n) seeds.add(n)
    }
  } else if (line.endsWith("map:")) {
    currentMap = []
    maps.add(currentMap)
  } else {
    var parts = []
    for (part in line.split(" ")) {
      var n = Num.fromString(part)
      if (n) parts.add(n)
    }
    if (parts.count == 3) {
      currentMap.add(parts)
    }
  }
}

var minLoc = -1
for (seed in seeds) {
  var loc = seed
  for (m in maps) {
    for (r in m) {
      var dest = r[0]
      var src = r[1]
      var len = r[2]
      if (loc >= src && loc < src + len) {
        loc = dest + (loc - src)
        break
      }
    }
  }
  if (minLoc == -1 || loc < minLoc) minLoc = loc
}

System.print(minLoc)

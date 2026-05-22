
import "io" for File

var lines = File.read("input.txt").split("\n")
var dirs = {}
var path = []

for (rawLine in lines) {
  var line = rawLine.trim()
  if (line == "") continue
  var parts = line.split(" ")
  if (parts[0] == "$") {
    if (parts[1] == "cd") {
      var dest = parts[2]
      if (dest == "/") {
        path = []
      } else if (dest == "..") {
        if (path.count > 0) path.removeAt(-1)
      } else {
        path.add(dest)
      }
    }
  } else if (parts[0] != "dir") {
    var size = Num.fromString(parts[0])
    for (i in 0..path.count) {
      var parent = path[0...i].join("/")
      dirs[parent] = (dirs[parent] || 0) + size
    }
  }
}

var needed = 30000000 - (70000000 - dirs[""])
var ans = 70000000
for (size in dirs.values) {
  if (size >= needed && size < ans) {
    ans = size
  }
}
System.print(ans)

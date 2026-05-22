
import "io" for File

var s = {}
var cwd = []
for (line in File.read("input.txt").split("\n")) {
  var parts = line.trim().split(" ")
  if (parts[0] == "$") {
    if (parts[1] == "cd") {
      if (parts[2] == "/") {
        cwd = ["/"]
      } else if (parts[2] == "..") {
        cwd.removeAt(-1)
      } else {
        cwd.add(parts[2])
      }
    }
  } else if (parts[0] != "" && parts[0] != "dir") {
    var size = Num.fromString(parts[0])
    var path = ""
    for (dir in cwd) {
      path = path == "" ? dir : (path == "/" ? "/" + dir : path + "/" + dir)
      s[path] = (s[path] || 0) + size
    }
  }
}

var total = 0
for (size in s.values) {
  if (size <= 100000) total = total + size
}
System.print(total)

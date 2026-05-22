
import "io" for File

var contents = File.read("input.txt")
var programs = {}
var above = {}

for (line in contents.split("\n")) {
  var trimmed = line.trim()
  if (trimmed.count > 0) {
    var parts = trimmed.split(" -> ")
    var parent = parts[0].split(" ")[0]
    programs[parent] = true
    if (parts.count > 1) {
      for (child in parts[1].split(", ")) {
        above[child.trim()] = true
      }
    }
  }
}

for (p in programs.keys) {
  if (!above.containsKey(p)) {
    System.print(p)
  }
}

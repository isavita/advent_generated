import "io" for File

var input = File.read("input.txt")

var score = 0
var depth = 0
var inGarbage = false
var cancelNext = false
var garbageCount = 0

for (line in input.split("\n")) {
  for (char in line) {
    if (cancelNext) {
      cancelNext = false
      continue
    }

    if (inGarbage) {
      if (char == "!") {
        cancelNext = true
      } else if (char == ">") {
        inGarbage = false
      } else {
        garbageCount = garbageCount + 1
      }
    } else {
      if (char == "{") {
        depth = depth + 1
      } else if (char == "}") {
        score = score + depth
        depth = depth - 1
      } else if (char == "<") {
        inGarbage = true
      }
    }
  }
}

System.print(garbageCount)
import "io" for File

var file = File.read("input.txt")
var input = file.trim()
var floor = 0
var position = 0

for (i in 0..input.count - 1) {
  if (input[i] == "(") {
    floor = floor + 1
  } else if (input[i] == ")") {
    floor = floor - 1
  }
  if (floor == -1) {
    position = i + 1
    break
  }
}

System.print(position)
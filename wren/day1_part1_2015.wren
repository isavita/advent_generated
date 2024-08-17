import "io" for File

var file = File.read("input.txt")
var input = file.trim()
var floor = 0
for (char in input) {
  if (char == "(") {
    floor = floor + 1
  } else if (char == ")") {
    floor = floor - 1
  }
}
System.print(floor)
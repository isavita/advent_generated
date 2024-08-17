import "io" for File

var input = File.read("input.txt").trim()
var lines = input.split("\n")
var offsets = []

for (line in lines) {
  offsets.add(Num.fromString(line))
}

var index = 0
var steps = 0

while (index >= 0 && index < offsets.count) {
  var jump = offsets[index]

  if (jump >= 3) {
    offsets[index] = offsets[index] - 1
  } else {
    offsets[index] = offsets[index] + 1
  }

  index = index + jump
  steps = steps + 1
}

System.print(steps)
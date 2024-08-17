import "io" for File

var input = File.read("input.txt")
var lines = input.split("\n")
var prev = 0
var current = 0
var count = 0

for (line in lines) {
  current = Num.fromString(line)
  if (prev != 0 && current > prev) {
    count = count + 1
  }
  prev = current
}

System.print(count)
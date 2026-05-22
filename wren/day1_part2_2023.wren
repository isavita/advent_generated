
import "io" for File

var content = File.read("input.txt")
var total = 0
var reps = [
  ["one", "o1e"],
  ["two", "t2o"],
  ["three", "t3e"],
  ["four", "f4r"],
  ["five", "f5e"],
  ["six", "s6x"],
  ["seven", "s7n"],
  ["eight", "e8t"],
  ["nine", "n9e"]
]

var process = Fn.new { |line|
  for (r in reps) {
    line = line.replace(r[0], r[1])
  }
  var digits = []
  for (c in line) {
    if ("0123456789".contains(c)) {
      digits.add(c)
    }
  }
  if (digits.count > 0) {
    return Num.fromString(digits[0]) * 10 + Num.fromString(digits[-1])
  }
  return 0
}

var line = ""
for (c in content) {
  if (c == "\n") {
    total = total + process.call(line)
    line = ""
  } else if (c != "\r") {
    line = line + c
  }
}
if (line.count > 0) {
  total = total + process.call(line)
}

System.print(total)

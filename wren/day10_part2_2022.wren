
import "io" for File

var lines = File.read("input.txt").trim().split("\n")
var x = [1]
for (line in lines) {
  if (line == "noop") {
    x.add(x[-1])
  } else {
    var parts = line.split(" ")
    var n = Num.fromString(parts[1])
    x.add(x[-1])
    x.add(x[-1] + n)
  }
}

for (y in 0...6) {
  var row = ""
  for (col in 0...40) {
    var idx = y * 40 + col
    var val = x[idx]
    if ((col - val).abs <= 1) {
      row = row + "#"
    } else {
      row = row + "."
    }
  }
  System.print(row)
}

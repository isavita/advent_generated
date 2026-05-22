
import "io" for File

var content = File.read("input.txt").trim()
var counts = List.filled(9, 0)
for (part in content.split(",")) {
  var val = Num.fromString(part.trim())
  counts[val] = counts[val] + 1
}

for (day in 1..256) {
  var newFish = counts[0]
  for (i in 0..7) {
    counts[i] = counts[i + 1]
  }
  counts[6] = counts[6] + newFish
  counts[8] = newFish
}

var total = 0
for (c in counts) {
  total = total + c
}
System.print(total)

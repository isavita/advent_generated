
import "io" for File

var content = File.read("input.txt")
var banks = []
var cur = ""
for (c in content) {
  if (c == " " || c == "\t" || c == "\r" || c == "\n") {
    if (cur != "") {
      banks.add(Num.fromString(cur))
      cur = ""
    }
  } else {
    cur = cur + c
  }
}
if (cur != "") banks.add(Num.fromString(cur))

var n = banks.count
var seen = {}
var cycles = 0

while (true) {
  var key = banks.join(",")
  if (seen.containsKey(key)) {
    System.print("Part 1: %(cycles)")
    System.print("Part 2: %(cycles - seen[key])")
    break
  }
  seen[key] = cycles

  var maxVal = -1
  var maxIdx = -1
  for (i in 0...n) {
    if (banks[i] > maxVal) {
      maxVal = banks[i]
      maxIdx = i
    }
  }

  banks[maxIdx] = 0
  var idx = maxIdx
  for (j in 0...maxVal) {
    idx = (idx + 1) % n
    banks[idx] = banks[idx] + 1
  }
  cycles = cycles + 1
}

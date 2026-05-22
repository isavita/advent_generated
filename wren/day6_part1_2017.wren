
import "io" for File

var input = File.read("input.txt").trim().replace("\t", " ")
var banks = []
for (x in input.split(" ")) {
  if (x != "") banks.add(Num.fromString(x))
}

var seen = {}
var cycles = 0
while (true) {
  var state = banks.join(",")
  if (seen.containsKey(state)) break
  seen[state] = true

  var maxIdx = 0
  for (i in 1...banks.count) {
    if (banks[i] > banks[maxIdx]) maxIdx = i
  }

  var blocks = banks[maxIdx]
  banks[maxIdx] = 0
  var idx = maxIdx
  while (blocks > 0) {
    idx = (idx + 1) % banks.count
    banks[idx] = banks[idx] + 1
    blocks = blocks - 1
  }
  cycles = cycles + 1
}

System.print("It takes %(cycles) redistribution cycles to reach a repeated configuration.")

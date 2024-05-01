import strutils, sequtils, algorithm

proc countArrangements(adapters: seq[int]): int64 =
  var ways = newSeqWith(adapters.len, 0'i64)
  ways[0] = 1'i64

  for i in 1..<adapters.len:
    let currentJoltage = adapters[i]
    for diff in [1, 2, 3]:
      let idx = binarySearch(adapters, currentJoltage - diff)
      if idx >= 0 and idx < adapters.len:
        ways[i] += ways[idx]

  return ways[^1]

let file = open("input.txt", fmRead)
var adapters = @[0]

for line in file.lines:
  adapters.add parseInt(line)

adapters.sort(system.cmp[int])

adapters.add adapters[^1] + 3

echo countArrangements(adapters)
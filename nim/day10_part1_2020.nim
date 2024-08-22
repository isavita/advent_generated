import sequtils, strutils, algorithm

let input = readFile("input.txt").splitLines().mapIt(parseInt(it))
let sortedAdapters = input.sorted()
let deviceJoltage = sortedAdapters[^1] + 3
let adapters = @[0] & sortedAdapters & @[deviceJoltage]

var diff1 = 0
var diff3 = 0
for i in 1..<adapters.len:
  let diff = adapters[i] - adapters[i-1]
  if diff == 1:
    inc diff1
  elif diff == 3:
    inc diff3

echo "1-jolt differences: ", diff1
echo "3-jolt differences: ", diff3
echo "Product: ", diff1 * diff3
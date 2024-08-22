import strutils, sequtils

let input = readFile("input.txt").strip()
var signal: seq[int] = newSeq[int](input.len)
for i, c in input:
  signal[i] = parseInt($c)

let basePattern = @[0, 1, 0, -1]

for _ in 0..<100:
  var newSignal: seq[int] = newSeq[int](signal.len)
  for i in 0..<signal.len:
    var sum = 0
    for j in 0..<signal.len:
      let patternIndex = ((j + 1) div (i + 1)) mod basePattern.len
      sum += signal[j] * basePattern[patternIndex]
    newSignal[i] = abs(sum) mod 10
  signal = newSignal

for i in 0..<8:
  stdout.write($signal[i])
stdout.write("\n")
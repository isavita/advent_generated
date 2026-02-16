import std/[os, strutils, sequtils]

func ceilDiv(a, b: int64): int64 = (a + b - 1) div b

proc pow10(k: int): int64 =
  var r: int64 = 1
  for i in 0..<k: r *= 10
  r

let data = readFile("input.txt")
  .replace("\r\n", "")
  .replace("\n", "")
  .strip()

var ranges: seq[(int64, int64)] = @[]
for p in data.split(','):
  let sp = p.split('-')
  ranges.add((sp[0].parseInt.int64, sp[1].parseInt.int64))

var total: int64 = 0

let p10 = (0..10).mapIt(pow10(it))

for k in 1..10:
  let mul = p10[k] + 1
  let minSeed = p10[k-1]
  let maxSeed = p10[k] - 1
  for (st, en) in ranges:
    let sMin = ceilDiv(st, mul)
    let sMax = en div mul
    let fromV = max(sMin, minSeed)
    let toV   = min(sMax, maxSeed)
    if fromV <= toV:
      let cnt = toV - fromV + 1
      total += mul * (fromV + toV) * cnt div 2

echo total

import os, strutils, algorithm

type Range = tuple[min, max: int64]

proc solve() =
  if not fileExists("input.txt"): return

  var ranges: seq[Range] = @[]
  for line in lines("input.txt"):
    let parts = line.split('-')
    if parts.len < 2: continue
    let a = parts[0].strip().parseBiggestInt()
    let b = parts[1].strip().parseBiggestInt()
    ranges.add((min(a, b), max(a, b)))

  if ranges.len == 0:
    echo "Total fresh IDs: 0"
    return

  ranges.sort()

  var total: int64 = 0
  var currentMin = ranges[0].min
  var currentMax = ranges[0].max

  for i in 1..<ranges.len:
    if ranges[i].min <= currentMax:
      currentMax = max(currentMax, ranges[i].max)
    else:
      total += (currentMax - currentMin + 1)
      currentMin = ranges[i].min
      currentMax = ranges[i].max
  
  total += (currentMax - currentMin + 1)
  echo "Total fresh IDs: ", total

solve()


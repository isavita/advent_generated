
import os
import strutils
import tables
import sequtils

proc num(w: seq[int]): BiggestInt =
  result = 0
  for digit in w:
    result = result * 10 + digit.BiggestInt

proc main() =
  let content = readFile("input.txt").strip()
  let lines = content.splitLines()

  var k, l, m: seq[int]

  for i, line in lines.pairs:
    let mod18 = i mod 18
    if mod18 == 4 or mod18 == 5 or mod18 == 15:
      let parts = line.split()
      if parts.len == 3:
        let v = parseInt(parts[2])
        case mod18
        of 4: l.add(v)
        of 5: k.add(v)
        of 15: m.add(v)
        else: discard

  var constraints: Table[int, array[2, int]]
  var stack: seq[int]

  for i in 0 ..< l.len:
    if l[i] == 1:
      stack.add(i)
    elif l[i] == 26:
      let popIdx = stack.pop()
      let delta = m[popIdx] + k[i]
      constraints[popIdx] = [i, delta]

  var minVal = newSeq[int](14) # Initialize with 14 zeros

  for popIdx, data in constraints.pairs:
    let iIdx = data[0]
    let delta = data[1]
    var vmin = 1
    while vmin + delta < 1:
      inc vmin
    minVal[popIdx] = vmin
    minVal[iIdx] = vmin + delta

  echo num(minVal)

when isMainModule:
  main()

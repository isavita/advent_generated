
import strutils
import sequtils
import tables
import os

proc num(w: openArray[int]): int64 =
  result = 0
  for digit in w:
    result = result * 10 + int64(digit)

proc main() =
  var k, l, m: seq[int]
  let lines = readFile("input.txt").splitLines()

  for i, line in lines:
    let parts = line.split()
    if parts.len > 0:
      let mod18 = i mod 18
      if mod18 == 4:
        l.add parseInt(parts[^1])
      elif mod18 == 5:
        k.add parseInt(parts[^1])
      elif mod18 == 15:
        m.add parseInt(parts[^1])

  var constraints = initTable[int, tuple[j: int, diff: int]]()
  var stack: seq[int]

  for i in 0 ..< l.len:
    if l[i] == 1:
      stack.add(i)
    elif l[i] == 26:
      let popIdx = stack.pop()
      constraints[popIdx] = (j: i, diff: m[popIdx] + k[i])

  var maxVals: array[14, int]

  for i in constraints.keys:
    let constraint = constraints[i]
    let j = constraint.j
    let diff = constraint.diff

    if diff >= 0:
      maxVals[i] = 9 - diff
      maxVals[j] = 9
    else:
      maxVals[i] = 9
      maxVals[j] = 9 + diff

  echo num(maxVals)

when isMainModule:
  main()

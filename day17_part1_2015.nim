
import os
import strutils

proc countCombinations(containers: seq[int], target, index: int): int =
  if target == 0:
    result = 1
  elif target < 0 or index >= containers.len:
    result = 0
  else:
    result = countCombinations(containers, target - containers[index], index + 1) +
             countCombinations(containers, target, index + 1)

var containers: seq[int] = @[]
let file = open("input.txt", fmRead)
for line in file.lines:
  containers.add(parseInt(line))

echo countCombinations(containers, 150, 0)

import strutils, sequtils

proc findCombinations(containers: seq[int], target, index, count: int, minCount, ways: var int) =
  if target == 0:
    if minCount == 0 or count < minCount:
      minCount = count
      ways = 1
    elif count == minCount:
      ways.inc
    return
  if target < 0 or index >= containers.len:
    return
  findCombinations(containers, target - containers[index], index + 1, count + 1, minCount, ways)
  findCombinations(containers, target, index + 1, count, minCount, ways)

when isMainModule:
  let file = open("input.txt")
  var containers: seq[int]
  for line in file.lines:
    containers.add parseInt(line)
  file.close()  # Close the file manually

  var minCount, ways: int
  findCombinations(containers, 150, 0, 0, minCount, ways)
  echo ways
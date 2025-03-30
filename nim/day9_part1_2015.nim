
import strutils, tables, sets, sequtils, algorithm, math, os

proc main() =
  var distances = initTable[(string, string), int]()
  var locationsSet = initHashSet[string]()

  let filename = "input.txt"
  if not fileExists(filename):
    echo "Error: input.txt not found"
    return

  for line in lines(filename):
    let parts = line.strip().split()
    if parts.len == 5:
      let loc1 = parts[0]
      let loc2 = parts[2]
      let dist = parseInt(parts[4])
      distances[(loc1, loc2)] = dist
      distances[(loc2, loc1)] = dist
      locationsSet.incl(loc1)
      locationsSet.incl(loc2)

  var locationsSeq = toSeq(items(locationsSet))

  var minDistance = high(int)

  if locationsSeq.len > 1:
    locationsSeq.sort(cmp[string]) # nextPermutation requires sorted sequence initially
    block permutationLoop:
      while true:
        var currentDistance = 0
        for i in 0 ..< locationsSeq.len - 1:
          currentDistance += distances[(locationsSeq[i], locationsSeq[i+1])]
        minDistance = min(minDistance, currentDistance)

        if not locationsSeq.nextPermutation():
          break permutationLoop
  elif locationsSeq.len <= 1:
      minDistance = 0 # Distance is 0 if 0 or 1 locations

  echo minDistance

main()

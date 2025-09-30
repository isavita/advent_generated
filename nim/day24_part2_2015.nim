
import sequtils, strutils, math, algorithm

proc readInput(filename: string): seq[int] =
  var file = open(filename)
  for line in file.lines:
    result.add(parseInt(line))
  close(file)

proc canPartition(weights: seq[int], target: int, groups: int): bool =
  if groups == 1:
    return true
  var weights = weights
  for i in 0 ..< weights.len:
    if weights[i] == 0:
      continue
    let current = weights[i]
    if current > target:
      return false
    weights[i] = 0
    if current == target:
      if canPartition(weights, target, groups-1):
        return true
    else:
      for j in (i+1) ..< weights.len:
        if weights[j] != 0 and current + weights[j] <= target:
          var newWeights = weights
          newWeights[j] = 0
          if canPartition(newWeights, target - current, groups-1):
            return true
    weights[i] = current
    return false
  return false

proc findMinQE(weights: seq[int], target: int, groups: int): int =
  let total = sum(weights)
  let n = weights.len
  var sortedWeights = weights.sortedByIt(-it)
  var minSize = int.high
  var bestQE = int.high

  proc dfs(start: int, currentSum: int, currentSize: int, currentProduct: int, used: var seq[bool]) =
    if currentSize > minSize:
      return
    if currentSum == target:
      var remaining: seq[int]
      for i in 0 ..< n:
        if not used[i]:
          remaining.add(sortedWeights[i])
      if canPartition(remaining, target, groups-1):
        if currentSize < minSize:
          minSize = currentSize
          bestQE = currentProduct
        elif currentSize == minSize and currentProduct < bestQE:
          bestQE = currentProduct
      return
    if start >= n:
      return
    if currentSum + sortedWeights[start] <= target:
      used[start] = true
      dfs(start+1, currentSum + sortedWeights[start], currentSize+1, currentProduct * sortedWeights[start], used)
      used[start] = false
    dfs(start+1, currentSum, currentSize, currentProduct, used)

  var used = newSeq[bool](n)
  for size in 1..n:
    if size > minSize:
      break
    dfs(0, 0, 0, 1, used)
  return bestQE

proc main =
  let weights = readInput("input.txt")
  let total = sum(weights)
  let target3 = total div 3
  let target4 = total div 4

  echo "Part One: ", findMinQE(weights, target3, 3)
  echo "Part Two: ", findMinQE(weights, target4, 4)

when isMainModule:
  main()

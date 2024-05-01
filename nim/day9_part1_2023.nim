import strutils, sequtils, strformat

proc parseInput(input: seq[string]): seq[seq[int]] =
  for line in input:
    let numbers = line.split.map(parseInt)
    result.add numbers

proc allZeros(nums: seq[int]): bool =
  for num in nums:
    if num != 0:
      return false
  true

proc calculateExtrapolation(history: seq[int]): seq[int] =
  for i in 1..<history.len:
    result.add history[i] - history[i-1]

proc calculateExtrapolations(history: seq[int]): seq[seq[int]] =
  result.add history
  for i in 1..<history.len:
    let previousExtrapolations = result[i-1]
    if allZeros(previousExtrapolations):
      break
    let extrapolations = calculateExtrapolation(previousExtrapolations)
    result.add extrapolations

proc solve(input: seq[string]): int =
  let histories = parseInput(input)
  for history in histories:
    let extrapolationsSeries = calculateExtrapolations(history)
    var futurePrediction = 0
    for i in countdown(extrapolationsSeries.len-1, 0):
      futurePrediction += extrapolationsSeries[i][^1]
    result += futurePrediction

when isMainModule:
  let input = readFile("input.txt").strip.splitLines
  echo solve(input)
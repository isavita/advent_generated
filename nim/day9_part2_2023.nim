
import strutils, sequtils, os

proc parseStringToInts(numbersLine: string): seq[int] =
  numbersLine.splitWhitespace.mapIt(parseInt(it))

proc allZeros(nums: seq[int]): bool =
  for num in nums:
    if num != 0:
      return false
  return true

proc calculateExtrapolation(history: seq[int]): seq[int] =
  result = newSeq[int](history.len - 1)
  for i in 1..<history.len:
    result[i-1] = history[i] - history[i-1]

proc calculateExtrapolations(history: seq[int]): seq[seq[int]] =
  result = @[history]
  for i in 1..<history.len:
    let previousExtrapolations = result[i-1]
    if allZeros(previousExtrapolations):
      return result
    result.add(calculateExtrapolation(previousExtrapolations))

proc solve(input: seq[string]): int =
  for history in input.mapIt(parseStringToInts(it)):
    let extrapolationsSeries = calculateExtrapolations(history)
    var pastPrediction = 0
    for i in countdown(extrapolationsSeries.len - 1, 0):
      pastPrediction = extrapolationsSeries[i][0] - pastPrediction
    result += pastPrediction

proc main() =
  let input = readFile("input.txt").splitLines
  echo solve(input)

when isMainModule:
  main()

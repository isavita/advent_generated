import sequtils, strutils, tables

proc rambunctiousRecitation(startingNumbers: seq[int], n: int): int =
  var
    lastSpoken = initTable[int, int]()
    turn = 1
    lastNumber = 0

  for number in startingNumbers:
    lastSpoken[number] = turn
    lastNumber = number
    turn.inc

  while turn <= n:
    if lastSpoken.hasKey(lastNumber):
      let diff = turn - lastSpoken[lastNumber] - 1
      lastSpoken[lastNumber] = turn - 1
      lastNumber = diff
    else:
      lastSpoken[lastNumber] = turn - 1
      lastNumber = 0

    turn.inc

  lastNumber

when isMainModule:
  let input = readFile("input.txt").strip().split(',').map(parseInt)
  echo "2020th number spoken: ", rambunctiousRecitation(input, 2020)
  echo "30000000th number spoken: ", rambunctiousRecitation(input, 30000000)
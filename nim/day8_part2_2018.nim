import strutils, sequtils, strformat

proc readInput(filename: string): seq[int] =
  let file = readFile(filename)
  let parts = file.splitWhitespace()
  result = newSeq[int]()
  for part in parts:
    result.add parseInt(part)

proc parseTree(data: seq[int], index: var int): int =
  let childCount = data[index]
  let metaCount = data[index + 1]
  index += 2

  var childValues: seq[int] = @[]
  for _ in 0 ..< childCount:
    var childValue: int
    childValue = parseTree(data, index)
    childValues.add childValue

  var value: int = 0
  if childCount == 0:
    for i in 0 ..< metaCount:
      value += data[index + i]
  else:
    for i in 0 ..< metaCount:
      let metadata = data[index + i]
      if metadata <= childCount and metadata > 0:
        value += childValues[metadata - 1]
  index += metaCount
  return value

let numbers = readInput("input.txt")
var index = 0
let value = parseTree(numbers, index)
echo fmt"Answer: {value}"
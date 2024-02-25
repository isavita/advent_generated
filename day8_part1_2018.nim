
import strutils

proc readInput(filename: string): seq[int] =
  let
    file = open(filename)
    line = file.readLine()
    parts = line.split(' ')
  result = newSeq[int](len(parts))
  for i, part in parts:
    result[i] = parseInt(part)
  file.close()
  return result

proc parseTree(data: seq[int], index: int): (int, int) =
  var childCount = data[index]
  var metaCount = data[index + 1]
  var newIndex = index + 2
  var sum = 0
  for i in 0 ..< childCount:
    let (childSum, updatedIndex) = parseTree(data, newIndex)
    sum += childSum
    newIndex = updatedIndex
  for i in 0 ..< metaCount:
    sum += data[newIndex + i]
  newIndex += metaCount
  return (sum, newIndex)

let numbers = readInput("input.txt")
let (sum, _) = parseTree(numbers, 0)
echo sum

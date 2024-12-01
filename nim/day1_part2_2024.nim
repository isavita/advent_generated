import std/[strutils, tables]

let content = readFile("input.txt")
var leftList: seq[int]
var rightNumbers = initCountTable[int]()

for line in content.splitLines():
  if line.len == 0: continue
  let numbers = line.splitWhitespace()
  if numbers.len >= 2:
    leftList.add(parseInt(numbers[0]))
    rightNumbers.inc(parseInt(numbers[^1]))

var similarityScore = 0
for num in leftList:
  similarityScore += num * rightNumbers[num]

echo "Similarity score: ", similarityScore

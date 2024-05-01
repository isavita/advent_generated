import strutils, sequtils, strformat

const
  totalCups = 1000000
  totalMoves = 10000000

var
  cups = newSeq[int](totalCups + 1)
  file = readFile("input.txt")
  input = file.strip()
  lastCup: int

for i, char in input:
  let cup = parseInt($char)
  if i > 0:
    cups[lastCup] = cup
  lastCup = cup

for i in countup(input.len + 1, totalCups):
  cups[lastCup] = i
  lastCup = i
cups[lastCup] = parseInt($input[0])

var currentCup = parseInt($input[0])
for _ in 1..totalMoves:
  let
    pickup1 = cups[currentCup]
    pickup2 = cups[pickup1]
    pickup3 = cups[pickup2]
  cups[currentCup] = cups[pickup3]
  var destinationCup = currentCup - 1
  if destinationCup == 0:
    destinationCup = totalCups
  while destinationCup == pickup1 or destinationCup == pickup2 or destinationCup == pickup3:
    dec destinationCup
    if destinationCup == 0:
      destinationCup = totalCups
  cups[pickup3] = cups[destinationCup]
  cups[destinationCup] = pickup1
  currentCup = cups[currentCup]

let cup1 = cups[1]
let cup2 = cups[cup1]
echo fmt"{cup1 * cup2}"
import strutils, sequtils

let file = readFile("input.txt")
let input = file.strip()

var cups = newSeq[int](input.len + 1)
var currentCup: int
for i, char in input:
  let cup = parseInt($char)
  if i == 0:
    currentCup = cup
  if i < input.len - 1:
    let nextCup = parseInt($input[i + 1])
    cups[cup] = nextCup
var firstCup = parseInt($input[0])
var lastCup = parseInt($input[input.len - 1])
cups[lastCup] = firstCup

for _ in 0..<100:
  let pickup1 = cups[currentCup]
  let pickup2 = cups[pickup1]
  let pickup3 = cups[pickup2]

  cups[currentCup] = cups[pickup3]

  var destinationCup = currentCup - 1
  if destinationCup < 1:
    destinationCup = input.len
  while destinationCup == pickup1 or destinationCup == pickup2 or destinationCup == pickup3:
    dec destinationCup
    if destinationCup < 1:
      destinationCup = input.len

  cups[pickup3] = cups[destinationCup]
  cups[destinationCup] = pickup1

  currentCup = cups[currentCup]

var cup = cups[1]
while cup != 1:
  write(stdout, $cup)
  cup = cups[cup]
  if cup == 1:
    break
echo()
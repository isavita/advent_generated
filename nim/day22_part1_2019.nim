
import strutils, sequtils, os

const Size = 10007

proc dealIntoNewStack(deck: var seq[int]) =
  for i in 0 ..< Size div 2:
    swap(deck[i], deck[Size - i - 1])

proc cutN(deck: var seq[int], n: int) =
  if n >= 0:
    deck = deck[n ..< Size] & deck[0 ..< n]
  else:
    deck = deck[Size + n ..< Size] & deck[0 ..< Size + n]

proc dealWithIncrement(deck: var seq[int], n: int) =
  var newDeck = newSeq[int](Size)
  for i in 0 ..< Size:
    newDeck[(i * n) mod Size] = deck[i]
  deck = newDeck

proc find2019(deck: seq[int]): int =
  for i in 0 ..< Size:
    if deck[i] == 2019:
      return i
  return -1

var deck = toSeq(0 ..< Size)
let file = open("input.txt")
for line in file.lines:
  if line == "deal into new stack":
    dealIntoNewStack(deck)
  elif line.startsWith("cut"):
    let n = parseInt(line.split(' ')[1])
    cutN(deck, n)
  elif line.startsWith("deal with increment"):
    let n = parseInt(line.split(' ')[^1])
    dealWithIncrement(deck, n)
close(file)

echo find2019(deck)

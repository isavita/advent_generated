import strutils, sequtils, algorithm

type Marble = ref object
  value: int
  prev: Marble
  next: Marble

proc readInput(filename: string): (int, int) =
  let file = readFile(filename)
  let parts = file.splitWhitespace()
  let players = parseInt(parts[0])
  let lastMarble = parseInt(parts[6])
  (players, lastMarble)

proc playMarbleGame(players, lastMarble: int): int =
  var scores = newSeq[int](players)
  var current = Marble(value: 0)
  current.prev = current
  current.next = current

  for marble in 1..lastMarble:
    if marble mod 23 == 0:
      let player = marble mod players
      for _ in 1..7:
        current = current.prev
      scores[player] += marble + current.value
      current.prev.next = current.next
      current.next.prev = current.prev
      current = current.next
    else:
      current = current.next
      let newMarble = Marble(value: marble, prev: current, next: current.next)
      current.next.prev = newMarble
      current.next = newMarble
      current = newMarble

  var maxScore = 0
  for score in scores:
    if score > maxScore:
      maxScore = score
  maxScore

let (players, lastMarble) = readInput("input.txt")
let newLastMarble = lastMarble * 100
echo playMarbleGame(players, newLastMarble)
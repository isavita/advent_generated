import std/[sequtils, strutils, algorithm]

type
  Card = char
  Hand = seq[Card]
  HandWithBid = tuple[hand: Hand, bid: int]

proc cardValue(card: Card): int =
  case card
  of 'A': 14
  of 'K': 13
  of 'Q': 12
  of 'J': 11
  of 'T': 10
  else: parseInt($card)

proc handStrength(hand: Hand): int =
  var counts = newSeq[int](15)
  for card in hand:
    inc counts[cardValue(card)]
  counts.sort(cmp = proc(a, b: int): int = cmp(b, a))  # Sort descending
  case counts[0]
  of 5: 7  # Five of a kind
  of 4: 6  # Four of a kind
  of 3:
    if counts[1] == 2: 5  # Full house
    else: 4  # Three of a kind
  of 2:
    if counts[1] == 2: 3  # Two pair
    else: 2  # One pair
  else: 1  # High card

proc compareHands(a, b: Hand): int =
  for i in 0..<len(a):
    let va = cardValue(a[i])
    let vb = cardValue(b[i])
    if va != vb:
      return cmp(va, vb)
  return 0

proc main() =
  let file = "input.txt"
  var hands: seq[HandWithBid] = @[]
  for line in lines(file):
    let parts = line.split()
    let hand = parts[0].toSeq()
    let bid = parseInt(parts[1])
    hands.add((hand, bid))

  hands.sort(cmp = proc(a, b: HandWithBid): int =
    let strengthA = handStrength(a.hand)
    let strengthB = handStrength(b.hand)
    if strengthA != strengthB:
      return cmp(strengthA, strengthB)
    compareHands(a.hand, b.hand)
  )

  var totalWinnings = 0
  for i, hand in hands:
    totalWinnings += hand.bid * (i + 1)

  echo totalWinnings

main()
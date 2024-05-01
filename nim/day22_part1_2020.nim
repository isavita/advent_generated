import sequtils, strutils

type
  Deck = seq[int]

proc parseDeck(s: string): Deck =
  let lines = s.splitLines()
  result = @[]
  for line in lines[1..^1]:
    result.add(line.parseInt())

proc combat(deck1, deck2: Deck): Deck =
  var deck1 = deck1
  var deck2 = deck2
  while deck1.len > 0 and deck2.len > 0:
    let card1 = deck1[0]
    let card2 = deck2[0]
    deck1.delete(0)
    deck2.delete(0)
    if card1 > card2:
      deck1.add(card1)
      deck1.add(card2)
    else:
      deck2.add(card2)
      deck2.add(card1)
  if deck1.len > 0:
    result = deck1
  else:
    result = deck2

proc score(deck: Deck): int =
  for i, card in deck.pairs:
    result += card * (deck.len - i)

let input = "input.txt".readFile().split("\n\n")
let deck1 = parseDeck(input[0])
let deck2 = parseDeck(input[1])
let winningDeck = combat(deck1, deck2)
echo "Winning player's score: ", score(winningDeck)
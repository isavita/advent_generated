import strutils, sequtils, tables

type Deck = seq[int]

proc parseDeck(lines: seq[string]): Deck =
  var deck: Deck = @[]
  for line in lines:
    deck.add(parseInt(line))
  return deck

proc decksEqual(deck1, deck2: Deck): bool =
  if deck1.len != deck2.len: return false
  for i in 0..<deck1.len:
    if deck1[i] != deck2[i]: return false
  return true

proc playRecursiveCombat(deck1, deck2: Deck, gameNum: int): (int, Deck) =
  var deck1 = deck1
  var deck2 = deck2
  var previousRounds: Table[string, bool] = initTable[string, bool]()

  while deck1.len > 0 and deck2.len > 0:
    let roundState = $deck1 & "|" & $deck2
    if previousRounds.hasKey(roundState):
      return (1, deck1)
    previousRounds[roundState] = true

    let card1 = deck1[0]
    let card2 = deck2[0]
    deck1.delete(0)
    deck2.delete(0)

    var winner: int
    if deck1.len >= card1 and deck2.len >= card2:
      let subDeck1 = deck1[0..<card1]
      let subDeck2 = deck2[0..<card2]
      let result = playRecursiveCombat(subDeck1, subDeck2, gameNum + 1)
      winner = result[0]
    else:
      winner = if card1 > card2: 1 else: 2

    if winner == 1:
      deck1.add(card1)
      deck1.add(card2)
    else:
      deck2.add(card2)
      deck2.add(card1)

  if deck1.len > 0:
    return (1, deck1)
  else:
    return (2, deck2)

proc calculateScore(deck: Deck): int =
  var score = 0
  for i, card in deck:
    score += card * (deck.len - i)
  return score

when isMainModule:
  let input = readFile("input.txt").split("\n\n")
  let deck1 = parseDeck(input[0].splitLines[1..^1])
  let deck2 = parseDeck(input[1].splitLines[1..^1])

  let (_, winningDeck) = playRecursiveCombat(deck1, deck2, 1)
  echo "Winning player's score: ", calculateScore(winningDeck)
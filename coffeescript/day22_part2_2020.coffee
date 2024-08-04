fs = require 'fs'

class Deck
  constructor: ->
    @cards = []

  copy: (n) ->
    copy = new Deck()
    for i in [0...n]
      copy.cards[i] = this.cards[i]
    copy

  score: ->
    score = 0
    for card, i in this.cards
      score += card * (this.cards.length - i)
    score

  push: (card) ->
    @cards.push card

  length: ->
    @cards.length

  concat: (cards) ->
    @cards = @cards.concat cards
    @cards

  slice: (start, end) ->
    @cards.slice start, end

  toString: ->
    @cards.toString()

playRecursiveCombat = (player1, player2) ->
  previousRounds = {}
  while player1.length() > 0 and player2.length() > 0
    roundKey = "#{player1.toString()}|#{player2.toString()}"
    if previousRounds[roundKey]
      return [player1, new Deck()]
    previousRounds[roundKey] = true

    [card1, card2] = [player1.cards[0], player2.cards[0]]
    player1.cards = player1.cards[1..]
    player2.cards = player2.cards[1..]

    if player1.length() >= card1 and player2.length() >= card2
      [subPlayer1, _] = playRecursiveCombat(player1.copy(card1), player2.copy(card2))
      if subPlayer1.length() > 0
        player1.concat([card1, card2])
      else
        player2.concat([card2, card1])
    else
      if card1 > card2
        player1.concat([card1, card2])
      else
        player2.concat([card2, card1])
  [player1, player2]

input = fs.readFileSync('input.txt', 'utf8').split('\n')
player1Deck = new Deck()
player2Deck = new Deck()
currentDeck = player1Deck

for line in input
  if line == ""
    currentDeck = player2Deck
    continue
  if line.includes "Player"
    continue
  currentDeck.push parseInt(line)

[player1Deck, player2Deck] = playRecursiveCombat(player1Deck, player2Deck)
winningDeck = if player1Deck.length() > 0 then player1Deck else player2Deck
console.log winningDeck.score()
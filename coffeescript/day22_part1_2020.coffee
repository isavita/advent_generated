fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  lines = data.trim().split '\n'
  player1Deck = []
  player2Deck = []
  currentDeck = null

  for line in lines
    if line == ""
      currentDeck = player2Deck
      continue
    if line.startsWith "Player"
      currentDeck = player1Deck if line.includes "1"
      continue
    card = parseInt line
    currentDeck.push card

  while player1Deck.length > 0 and player2Deck.length > 0
    card1 = player1Deck.shift()
    card2 = player2Deck.shift()
    if card1 > card2
      player1Deck.push card1, card2
    else
      player2Deck.push card2, card1

  winningDeck = if player1Deck.length > 0 then player1Deck else player2Deck
  score = 0
  for card, i in winningDeck
    score += card * (winningDeck.length - i)

  console.log score
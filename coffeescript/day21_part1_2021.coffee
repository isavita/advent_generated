fs = require 'fs'

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  lines = data.trim().split '\n'
  player1Start = parseInt lines[0].slice(28).trim()
  player2Start = parseInt lines[1].slice(28).trim()

  player1Pos = player1Start
  player2Pos = player2Start

  player1Score = 0
  player2Score = 0

  dieRoll = 1
  rollCount = 0

  while true
    # Player 1
    rolls = (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100)
    rollCount += 3
    dieRoll += 3

    player1Pos = ((player1Pos + rolls - 1) % 10) + 1
    player1Score += player1Pos

    if player1Score >= 1000
      console.log "Result:", player2Score * rollCount
      break

    # Player 2
    rolls = (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100)
    rollCount += 3
    dieRoll += 3

    player2Pos = ((player2Pos + rolls - 1) % 10) + 1
    player2Score += player2Pos

    if player2Score >= 1000
      console.log player1Score * rollCount
      break
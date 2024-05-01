import strutils, sequtils, math

let data = readFile("input.txt").splitLines()
let player1Start = parseInt(data[0][28..^1].strip)
let player2Start = parseInt(data[1][28..^1].strip)

var player1Pos = player1Start
var player2Pos = player2Start
var player1Score = 0
var player2Score = 0
var dieRoll = 1
var rollCount = 0

while true:
  # Player 1
  var rolls = (dieRoll mod 100) + ((dieRoll + 1) mod 100) + ((dieRoll + 2) mod 100)
  inc(rollCount, 3)
  inc(dieRoll, 3)

  player1Pos = ((player1Pos + rolls - 1) mod 10) + 1
  inc(player1Score, player1Pos)

  if player1Score >= 1000:
    echo "Result: ", player2Score * rollCount
    break

  # Player 2
  rolls = (dieRoll mod 100) + ((dieRoll + 1) mod 100) + ((dieRoll + 2) mod 100)
  inc(rollCount, 3)
  inc(dieRoll, 3)

  player2Pos = ((player2Pos + rolls - 1) mod 10) + 1
  inc(player2Score, player2Pos)

  if player2Score >= 1000:
    echo "Result: ", player1Score * rollCount
    break
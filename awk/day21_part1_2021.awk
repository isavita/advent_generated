
BEGIN {
  getline line1 < "input.txt"
  getline line2 < "input.txt"
  player1Pos = substr(line1, 29) + 0
  player2Pos = substr(line2, 29) + 0
  player1Score = 0
  player2Score = 0
  dieRoll = 1
  rollCount = 0
  while (1) {
    rolls = (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100)
    rollCount += 3
    dieRoll += 3
    player1Pos = (player1Pos + rolls - 1) % 10 + 1
    player1Score += player1Pos
    if (player1Score >= 1000) {
      print player2Score * rollCount
      exit
    }
    rolls = (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100)
    rollCount += 3
    dieRoll += 3
    player2Pos = (player2Pos + rolls - 1) % 10 + 1
    player2Score += player2Pos
    if (player2Score >= 1000) {
      print player1Score * rollCount
      exit
    }
  }
}

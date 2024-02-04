
data = open("input.txt", "r")
lines = data.readlines()
player1Start = int(lines[0][28:].strip())
player2Start = int(lines[1][28:].strip())
player1Pos = player1Start
player2Pos = player2Start
player1Score = 0
player2Score = 0
dieRoll = 1
rollCount = 0

while True:
    rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
    rollCount += 3
    dieRoll += 3

    player1Pos = (player1Pos + rolls - 1) % 10 + 1
    player1Score += player1Pos

    if player1Score >= 1000:
        print(player2Score * rollCount)
        break

    rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
    rollCount += 3
    dieRoll += 3

    player2Pos = (player2Pos + rolls - 1) % 10 + 1
    player2Score += player2Pos

    if player2Score >= 1000:
        print(player1Score * rollCount)
        break

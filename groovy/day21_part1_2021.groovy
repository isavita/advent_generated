
def lines = new File('input.txt').readLines()
def player1Start = lines[0][28..-1].trim() as Integer
def player2Start = lines[1][28..-1].trim() as Integer
def player1Pos = player1Start
def player2Pos = player2Start
def player1Score = 0
def player2Score = 0
def dieRoll = 1
def rollCount = 0

while (true) {
    def rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
    rollCount += 3
    dieRoll += 3

    player1Pos = (player1Pos + rolls - 1) % 10 + 1
    player1Score += player1Pos

    if (player1Score >= 1000) {
        println(player2Score * rollCount)
        break
    }

    rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
    rollCount += 3
    dieRoll += 3

    player2Pos = (player2Pos + rolls - 1) % 10 + 1
    player2Score += player2Pos

    if (player2Score >= 1000) {
        println(player1Score * rollCount)
        break
    }
}

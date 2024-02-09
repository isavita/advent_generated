import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    var player1Start = lines[0].substring(28).trim().toInt()
    var player2Start = lines[1].substring(28).trim().toInt()
    var player1Pos = player1Start
    var player2Pos = player2Start
    var player1Score = 0
    var player2Score = 0
    var dieRoll = 1
    var rollCount = 0

    while (true) {
        // Player 1
        var rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100
        rollCount += 3
        dieRoll += 3
        player1Pos = (player1Pos + rolls - 1) % 10 + 1
        player1Score += player1Pos

        if (player1Score >= 1000) {
            println("Result: ${player2Score * rollCount}")
            break
        }

        // Player 2
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
}
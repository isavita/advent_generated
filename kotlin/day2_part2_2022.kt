import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    var totalScore = 0

    for (line in lines) {
        val opponent = line[0]
        val roundEnd = line[2]

        var yourMove = ' '
        yourMove = when {
            roundEnd == 'X' -> when {
                opponent == 'A' -> 'Z'
                opponent == 'B' -> 'X'
                else -> 'Y'
            }
            roundEnd == 'Y' -> when {
                opponent == 'A' -> 'X'
                opponent == 'B' -> 'Y'
                else -> 'Z'
            }
            else -> when {
                opponent == 'A' -> 'Y'
                opponent == 'B' -> 'Z'
                else -> 'X'
            }
        }

        var score = when (yourMove) {
            'X' -> 1
            'Y' -> 2
            'Z' -> 3
            else -> 0
        }

        if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
            score += 6
        } else if ((opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')) {
            score += 3
        }

        totalScore += score
    }

    println(totalScore)
}
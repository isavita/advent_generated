import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    var totalScore = 0

    for (line in lines) {
        val opponent = line[0]
        val yourMove = line[2]

        var score = 0
        when (yourMove) {
            'X' -> score = 1
            'Y' -> score = 2
            'Z' -> score = 3
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
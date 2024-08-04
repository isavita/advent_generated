import java.io.File

fun main() {
    val inputFile = File("input.txt")
    val lines = inputFile.readLines()
    var totalPoints = 0

    for (line in lines) {
        val parts = line.split(": ", " | ")
        val winningNumbers = parts[1].split(" ").filter { it.isNotBlank() }.map { it.toInt() }.toSet()
        val myNumbers = parts[2].split(" ").filter { it.isNotBlank() }.map { it.toInt() }

        var matches = 0
        for (number in myNumbers) {
            if (number in winningNumbers) {
                matches++
            }
        }

        val points = if (matches > 0) 1 shl (matches - 1) else 0
        totalPoints += points
    }

    println("Total points: $totalPoints")
}
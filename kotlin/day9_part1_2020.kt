import java.io.File

fun main() {
    val input = File("input.txt").readLines().map { it.toLong() }
    val preambleLength = 25

    for (i in preambleLength until input.size) {
        val currentNumber = input[i]
        val previousNumbers = input.subList(i - preambleLength, i)
        val isValid = previousNumbers.any { first ->
            previousNumbers.any { second ->
                first != second && first + second == currentNumber
            }
        }

        if (!isValid) {
            println(currentNumber)
            return
        }
    }
}
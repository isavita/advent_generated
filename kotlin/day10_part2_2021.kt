import java.io.File

fun main() {
    val input = File("input.txt").readLines()

    val scoresPart1 = input.map { line ->
        val openingToClosing = mapOf('(' to ')', '[' to ']', '{' to '}', '<' to '>')
        val closingToScore = mapOf(')' to 3, ']' to 57, '}' to 1197, '>' to 25137)

        val stack = mutableListOf<Char>()
        for (char in line) {
            if (char in openingToClosing.keys) {
                stack.add(char)
            } else if (char in closingToScore.keys) {
                val openingChar = stack.removeLast()
                if (openingToClosing[openingChar] != char) {
                    return@map closingToScore[char]!!
                }
            }
        }
        0
    }.sum()

    println("Part 1: $scoresPart1")

    val incompleteLines = input.filter { line ->
        val openingToClosing = mapOf('(' to ')', '[' to ']', '{' to '}', '<' to '>')
        val stack = mutableListOf<Char>()
        for (char in line) {
            if (char in openingToClosing.keys) {
                stack.add(char)
            } else if (char in openingToClosing.values) {
                if (stack.isEmpty() || openingToClosing[stack.last()] != char) {
                    return@filter false
                }
                stack.removeLast()
            }
        }
        stack.isNotEmpty()
    }

    val scoresPart2 = incompleteLines.map { line ->
        val openingToClosing = mapOf('(' to ')', '[' to ']', '{' to '}', '<' to '>')
        val closingToScore = mapOf(')' to 1, ']' to 2, '}' to 3, '>' to 4)

        val stack = mutableListOf<Char>()
        for (char in line) {
            if (char in openingToClosing.keys) {
                stack.add(char)
            } else if (char in openingToClosing.values) {
                stack.removeLast()
            }
        }

        var score = 0L
        for (char in stack.reversed()) {
            score = score * 5 + closingToScore[openingToClosing[char]!!]!!
        }
        score
    }.sorted().let { it[it.size / 2] }

    println("Part 2: $scoresPart2")
}
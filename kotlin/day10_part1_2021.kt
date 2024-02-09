import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val totalScore = file.readLines()
        .fold(0) { acc, line ->
            val (score, corrupted) = checkLine(line)
            if (corrupted) acc + score else acc
        }
    println(totalScore)
}

fun checkLine(line: String): Pair<Int, Boolean> {
    val pairings = mapOf(')' to '(', ']' to '[', '}' to '{', '>' to '<')
    val scores = mapOf(')' to 3, ']' to 57, '}' to 1197, '>' to 25137)
    val stack = mutableListOf<Char>()

    line.forEach { char ->
        when (char) {
            '(', '[', '{', '<' -> stack.add(char)
            ')', ']', '}', '>' -> {
                if (stack.isEmpty() || stack.last() != pairings[char]) {
                    return Pair(scores[char]!!, true) // corrupted line
                }
                stack.removeAt(stack.size - 1) // pop from stack
            }
        }
    }
    return Pair(0, false) // line is not corrupted
}
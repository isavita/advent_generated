import java.io.File

fun main() {
    val polymer = File("input.txt").readText()
    val reactedPolymer = react(polymer)
    println("Units remaining: ${reactedPolymer.length}")
}

fun react(polymer: String): String {
    val stack = mutableListOf<Char>()
    for (c in polymer) {
        if (stack.isNotEmpty() && areOpposites(stack.last(), c)) {
            stack.removeAt(stack.lastIndex)
        } else {
            stack.add(c)
        }
    }
    return stack.joinToString("")
}

fun areOpposites(a: Char, b: Char): Boolean {
    return a != b && a.lowercaseChar() == b.lowercaseChar()
}
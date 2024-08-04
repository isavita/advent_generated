import java.io.File

fun main() {
    val input = File("input.txt").readText().trim()

    // Part 1: Fully react the polymer
    val fullyReactedPolymer = reactPolymer(input)
    println("Part 1: ${fullyReactedPolymer.length}")

    // Part 2: Find the shortest polymer by removing all units of exactly one type
    val shortestLength = ('a'..'z').minOf { char ->
        val modifiedPolymer = input.filter { it.toLowerCase() != char }
        reactPolymer(modifiedPolymer).length
    }
    println("Part 2: $shortestLength")
}

fun reactPolymer(polymer: String): String {
    val stack = mutableListOf<Char>()

    for (char in polymer) {
        if (stack.isNotEmpty() && stack.last().let { it != char && it.toLowerCase() == char.toLowerCase() }) {
            stack.removeAt(stack.lastIndex)
        } else {
            stack.add(char)
        }
    }

    return stack.joinToString("")
}
import java.io.File

fun main() {
    val input = File("input.txt").readLines().map { it.toInt() }

    // Part 1
    val part1Answer = input.zipWithNext { a, b -> if (b > a) 1 else 0 }.sum()
    println("Part 1 answer: $part1Answer")

    // Part 2
    val part2Answer = input.windowed(3, 1).map { it.sum() }.zipWithNext { a, b -> if (b > a) 1 else 0 }.sum()
    println("Part 2 answer: $part2Answer")
}
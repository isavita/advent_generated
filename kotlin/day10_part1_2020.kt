import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val adapters = file.readLines().map { it.toInt() }.sorted()

    var joltDifferences = mutableMapOf(3 to 1)
    var previousJoltage = 0

    adapters.forEach { adapter ->
        val diff = adapter - previousJoltage
        joltDifferences[diff] = joltDifferences.getOrDefault(diff, 0) + 1
        previousJoltage = adapter
    }

    val product = joltDifferences[1]!! * joltDifferences[3]!!
    println(product)
}
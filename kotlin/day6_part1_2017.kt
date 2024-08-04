import java.io.File
import java.util.HashSet

fun main() {
    // Read the input from the file
    val input = File("input.txt").readText().trim()
    val initialBanks = input.split("\t").map { it.toInt() }

    // Initialize variables
    val seenConfigurations = HashSet<List<Int>>()
    var currentBanks = initialBanks.toMutableList()
    var cycles = 0

    // Perform redistribution cycles until a repeated configuration is found
    while (!seenConfigurations.contains(currentBanks)) {
        seenConfigurations.add(currentBanks.toList())

        // Find the bank with the most blocks
        val maxBlocks = currentBanks.maxOrNull() ?: 0
        val maxIndex = currentBanks.indexOf(maxBlocks)

        // Redistribute the blocks
        currentBanks[maxIndex] = 0
        for (i in 1..maxBlocks) {
            currentBanks[(maxIndex + i) % currentBanks.size] += 1
        }

        cycles++
    }

    // Print the result
    println(cycles)
}
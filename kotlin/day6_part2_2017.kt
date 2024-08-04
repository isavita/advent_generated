import java.io.File

fun main() {
    val data = File("input.txt").readText().trim().split(Regex("\\s+"))
    val banks = data.map { it.toInt() }.toMutableList()

    val seen = mutableMapOf<List<Int>, Int>()
    var cycles = 0

    while (true) {
        val state = banks.toList()

        if (state in seen) {
            println("The size of the loop is ${cycles - seen[state]!!}")
            return
        }
        seen[state] = cycles

        var maxIndex = banks.indexOf(banks.maxOrNull()!!)
        val blocks = banks[maxIndex]
        banks[maxIndex] = 0
        for (i in 1..blocks) {
            banks[(maxIndex + i) % banks.size]++
        }

        cycles++
    }
}
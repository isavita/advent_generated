import java.io.File

fun main(args: Array<String>) {
    val adapters = mutableListOf(0)
    File("input.txt").forEachLine { adapters.add(it.toInt()) }
    adapters.sort()
    adapters.add(adapters.last() + 3)

    println(countArrangements(adapters))
}

fun countArrangements(adapters: List<Int>): Long {
    val ways = mutableMapOf<Int, Long>()
    ways[0] = 1

    for (i in 1 until adapters.size) {
        val currentJoltage = adapters[i]
        for (diff in listOf(1, 2, 3)) {
            ways[currentJoltage] = ways.getOrDefault(currentJoltage, 0) + ways.getOrDefault(currentJoltage - diff, 0)
        }
    }

    return ways[adapters.last()] ?: 0
}
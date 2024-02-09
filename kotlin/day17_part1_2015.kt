import java.io.File

fun main() {
    val containers = File("input.txt").readLines().map { it.toInt() }
    val target = 150

    fun findCombinations(index: Int, remaining: Int): Int {
        if (remaining == 0) return 1
        if (index == containers.size || remaining < 0) return 0

        return findCombinations(index + 1, remaining) + findCombinations(index + 1, remaining - containers[index])
    }

    println(findCombinations(0, target))
}
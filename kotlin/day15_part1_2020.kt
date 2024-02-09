import java.io.File

fun main() {
    val input = File("input.txt").readText().trim().split(",").map { it.toInt() }
    val map = mutableMapOf<Int, Int>()
    var lastNum = 0

    for (i in input.indices) {
        if (i < input.size - 1) {
            map[input[i]] = i + 1
        }
        lastNum = input[i]
    }

    for (i in input.size until 2020) {
        val nextNum = if (map.containsKey(lastNum)) i - map[lastNum]!! else 0
        map[lastNum] = i
        lastNum = nextNum
    }

    println(lastNum)
}
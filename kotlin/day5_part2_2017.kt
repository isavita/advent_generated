import java.io.File

fun main() {
    val jumps = File("input.txt").readLines().map { it.toInt() }.toMutableList()
    var index = 0
    var steps = 0

    while (index in jumps.indices) {
        val offset = jumps[index]
        jumps[index] += if (offset >= 3) -1 else 1
        index += offset
        steps++
    }

    println(steps)
}
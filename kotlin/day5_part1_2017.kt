import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val offsets = lines.map { it.toInt() }.toMutableList()

    var index = 0
    var steps = 0

    while (index in offsets.indices) {
        val jump = offsets[index]
        offsets[index]++
        index += jump
        steps++
    }

    println(steps)
}
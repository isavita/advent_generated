import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    var totalSquareFeet = 0

    for (line in input) {
        val dimensions = line.split("x").map { it.toInt() }
        val l = dimensions[0]
        val w = dimensions[1]
        val h = dimensions[2]

        val area = 2*l*w + 2*w*h + 2*h*l
        val slack = listOf(l*w, w*h, h*l).minOrNull() ?: 0

        totalSquareFeet += area + slack
    }

    println(totalSquareFeet)
}
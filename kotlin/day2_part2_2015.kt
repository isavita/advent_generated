import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    var totalWrappingPaper = 0
    var totalRibbon = 0

    for (line in lines) {
        val dimensions = line.split("x").map { it.toInt() }
        val l = dimensions[0]
        val w = dimensions[1]
        val h = dimensions[2]

        val side1 = l * w
        val side2 = w * h
        val side3 = h * l
        val slack = minOf(side1, side2, side3)

        totalWrappingPaper += 2 * side1 + 2 * side2 + 2 * side3 + slack
        totalRibbon += 2 * (l + w + h - maxOf(l, w, h)) + l * w * h
    }

    println(totalWrappingPaper)
    println(totalRibbon)
}
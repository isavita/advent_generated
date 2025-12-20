
import java.io.File

private fun floorDiv(a: Int, b: Int): Int {
    var q = a / b
    if (a < 0 && a % b != 0) q--
    return q
}

fun main() {
    val lines = File("input.txt").readLines()
    var currentPos = 50
    var totalZeroHits = 0
    val dialSize = 100

    for (raw in lines) {
        val line = raw.trim()
        if (line.isEmpty()) continue

        val direction = line[0]
        val amount = line.substring(1).toInt()

        when (direction) {
            'R' -> {
                val hits = (currentPos + amount) / dialSize
                totalZeroHits += hits
                currentPos = (currentPos + amount) % dialSize
            }
            'L' -> {
                val hits = floorDiv(currentPos - 1, dialSize) -
                           floorDiv(currentPos - amount - 1, dialSize)
                totalZeroHits += hits
                currentPos = (currentPos - amount) % dialSize
                if (currentPos < 0) currentPos += dialSize
            }
            else -> error("Unknown direction '$direction' in line '$line'")
        }
    }

    println("The password is: $totalZeroHits")
}

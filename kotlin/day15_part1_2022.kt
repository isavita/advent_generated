import java.io.File

data class Sensor(val x: Int, val y: Int, val beaconX: Int, val beaconY: Int) {
    val manhattanDistance: Int
        get() = Math.abs(x - beaconX) + Math.abs(y - beaconY)

    fun coverageInRow(y: Int): IntRange? {
        val distanceToRow = Math.abs(this.y - y)
        val remainingDistance = manhattanDistance - distanceToRow
        return if (remainingDistance >= 0) {
            val startX = x - remainingDistance
            val endX = x + remainingDistance
            IntRange(startX, endX)
        } else {
            null
        }
    }
}

fun main() {
    val input = File("input.txt").readLines()
    val sensors = input.map { line ->
        val parts = line.split(":", ",", "=").map { it.trim() }.filter { it.isNotEmpty() }
        Sensor(parts[1].toInt(), parts[3].toInt(), parts[5].toInt(), parts[7].toInt())
    }

    val targetY = 2000000
    val coverageRanges = mutableListOf<IntRange>()

    for (sensor in sensors) {
        sensor.coverageInRow(targetY)?.let { coverageRanges.add(it) }
    }

    // Merge overlapping ranges
    coverageRanges.sortBy { it.first }
    val mergedRanges = mutableListOf<IntRange>()
    var currentRange = coverageRanges[0]

    for (range in coverageRanges) {
        if (range.first <= currentRange.last + 1) {
            currentRange = IntRange(currentRange.first, Math.max(currentRange.last, range.last))
        } else {
            mergedRanges.add(currentRange)
            currentRange = range
        }
    }
    mergedRanges.add(currentRange)

    // Calculate the number of positions that cannot contain a beacon
    val beaconPositions = sensors.map { it.beaconX to it.beaconY }.toSet()
    val beaconsInRow = beaconPositions.count { it.second == targetY }
    val totalCoveredPositions = mergedRanges.sumOf { it.last - it.first + 1 }

    println(totalCoveredPositions - beaconsInRow)
}
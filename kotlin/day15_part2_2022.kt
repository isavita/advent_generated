import java.io.File
import kotlin.math.abs

data class Sensor(val x: Int, val y: Int, val beaconX: Int, val beaconY: Int) {
    val distance: Int = abs(x - beaconX) + abs(y - beaconY)

    fun covers(row: Int): IntRange? {
        val verticalDistance = abs(y - row)
        val remainingDistance = distance - verticalDistance
        if (remainingDistance < 0) return null
        return (x - remainingDistance)..(x + remainingDistance)
    }
}

fun parseInput(filename: String): List<Sensor> {
    val sensors = mutableListOf<Sensor>()
    File(filename).forEachLine { line ->
        val matchResult = Regex("Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)")
            .find(line)
        matchResult?.let {
            val (sx, sy, bx, by) = it.destructured
            sensors.add(Sensor(sx.toInt(), sy.toInt(), bx.toInt(), by.toInt()))
        }
    }
    return sensors
}

fun countCoveredPositions(sensors: List<Sensor>, row: Int): Int {
    val coveredRanges = mutableListOf<IntRange>()
    for (sensor in sensors) {
        sensor.covers(row)?.let { coveredRanges.add(it) }
    }
    coveredRanges.sortBy { it.first }

    var mergedRanges = mutableListOf<IntRange>()
    var currentRange = coveredRanges[0]
    for (range in coveredRanges.drop(1)) {
        if (range.first <= currentRange.last + 1) {
            currentRange = currentRange.first..maxOf(currentRange.last, range.last)
        } else {
            mergedRanges.add(currentRange)
            currentRange = range
        }
    }
    mergedRanges.add(currentRange)

    return mergedRanges.sumOf { it.last - it.first + 1 }
}

fun findDistressBeacon(sensors: List<Sensor>, maxCoord: Int): Long {
    for (y in 0..maxCoord) {
        val ranges = mutableListOf<IntRange>()
        for (sensor in sensors) {
            sensor.covers(y)?.let { ranges.add(it) }
        }
        ranges.sortBy { it.first }

        var x = 0
        for (range in ranges) {
            if (x < range.first) break
            x = maxOf(x, range.last + 1)
        }
        if (x <= maxCoord) {
            return x * 4000000L + y
        }
    }
    return -1
}

fun main() {
    val sensors = parseInput("input.txt")

    // Part One
    val row = 2000000
    val coveredPositions = countCoveredPositions(sensors, row)
    println("Part One: $coveredPositions")

    // Part Two
    val maxCoord = 4000000
    val tuningFrequency = findDistressBeacon(sensors, maxCoord)
    println("Part Two: $tuningFrequency")
}
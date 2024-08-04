import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val earliestTimestamp = input[0].toInt()
    val busIds = input[1].split(",").filter { it != "x" }.map { it.toInt() }

    // Part 1
    val (earliestBusId, waitTime) = findEarliestBus(earliestTimestamp, busIds)
    val part1Result = earliestBusId * waitTime
    println("Part 1: $part1Result")

    // Part 2
    val busSchedule = input[1].split(",").mapIndexedNotNull { index, id ->
        if (id != "x") index to id.toInt() else null
    }
    val part2Result = findEarliestTimestamp(busSchedule)
    println("Part 2: $part2Result")
}

fun findEarliestBus(earliestTimestamp: Int, busIds: List<Int>): Pair<Int, Int> {
    var minWaitTime = Int.MAX_VALUE
    var earliestBusId = -1

    for (busId in busIds) {
        val waitTime = busId - (earliestTimestamp % busId)
        if (waitTime < minWaitTime) {
            minWaitTime = waitTime
            earliestBusId = busId
        }
    }

    return earliestBusId to minWaitTime
}

fun findEarliestTimestamp(busSchedule: List<Pair<Int, Int>>): Long {
    var timestamp = 0L
    var step = 1L

    for ((offset, busId) in busSchedule) {
        while ((timestamp + offset) % busId != 0L) {
            timestamp += step
        }
        step *= busId
    }

    return timestamp
}
import java.io.File

data class Range(val start: Long, val end: Long)

fun main() {
    val inputFile = File("input.txt")
    val lines = inputFile.readLines()

    val ranges = lines.map { line ->
        val (start, end) = line.split("-").map { it.toLong() }
        Range(start, end)
    }.sortedBy { it.start }

    val mergedRanges = mergeRanges(ranges)

    val lowestAllowedIP = findLowestAllowedIP(mergedRanges)
    val totalAllowedIPs = findTotalAllowedIPs(mergedRanges)

    println("The lowest-valued IP that is not blocked is: $lowestAllowedIP")
    println("The total number of IPs allowed by the blacklist is: $totalAllowedIPs")
}

fun mergeRanges(ranges: List<Range>): List<Range> {
    val merged = mutableListOf<Range>()
    var current = ranges[0]

    for (i in 1 until ranges.size) {
        val next = ranges[i]
        if (current.end >= next.start - 1) {
            current = Range(current.start, maxOf(current.end, next.end))
        } else {
            merged.add(current)
            current = next
        }
    }
    merged.add(current)

    return merged
}

fun findLowestAllowedIP(ranges: List<Range>): Long {
    return if (ranges[0].start > 0) {
        0
    } else {
        ranges.firstOrNull { it.end + 1 != ranges[0].start }?.end?.plus(1) ?: 0
    }
}

fun findTotalAllowedIPs(ranges: List<Range>): Long {
    val maxIP = 4294967295L
    val blockedIPs = ranges.sumOf { it.end - it.start + 1L }
    return maxIP - blockedIPs + 1L
}
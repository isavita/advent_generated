import java.io.File

data class Range(val destinationStart: Long, val sourceStart: Long, val length: Long)

fun parseInput(filename: String): Pair<List<Pair<Long, Long>>, Map<String, List<Range>>> {
    val lines = File(filename).readLines()
    val seedsLine = lines[0].substringAfter("seeds: ").split(" ").map { it.toLong() }
    val seedRanges = seedsLine.chunked(2).map { it[0] to it[1] }

    val maps = mutableMapOf<String, List<Range>>()
    var currentMap: String? = null
    var currentRanges = mutableListOf<Range>()

    for (line in lines.drop(1)) {
        if (line.isEmpty()) {
            if (currentMap != null) {
                maps[currentMap] = currentRanges
                currentRanges = mutableListOf()
            }
            currentMap = null
        } else if (line.endsWith("map:")) {
            currentMap = line.substringBefore(" map:")
        } else {
            val (destStart, srcStart, length) = line.split(" ").map { it.toLong() }
            currentRanges.add(Range(destStart, srcStart, length))
        }
    }
    if (currentMap != null) {
        maps[currentMap] = currentRanges
    }

    return seedRanges to maps
}

fun mapValue(value: Long, ranges: List<Range>): Long {
    for (range in ranges) {
        if (value in range.sourceStart until range.sourceStart + range.length) {
            return range.destinationStart + (value - range.sourceStart)
        }
    }
    return value
}

fun mapRange(seedRanges: List<Pair<Long, Long>>, maps: Map<String, List<Range>>, mapOrder: List<String>): Long {
    var minLocation = Long.MAX_VALUE

    for ((seedStart, seedLength) in seedRanges) {
        var currentRanges = listOf(seedStart until seedStart + seedLength)

        for (mapName in mapOrder) {
            val mappedRanges = mutableListOf<LongRange>()
            val ranges = maps[mapName]!!

            for (range in currentRanges) {
                var start = range.first
                while (start < range.endInclusive) {
                    var mapped = false
                    for (mapping in ranges) {
                        if (start in mapping.sourceStart until mapping.sourceStart + mapping.length) {
                            val end = minOf(range.endInclusive, mapping.sourceStart + mapping.length - 1)
                            mappedRanges.add(mapping.destinationStart + (start - mapping.sourceStart) until mapping.destinationStart + (end - mapping.sourceStart) + 1)
                            start = end + 1
                            mapped = true
                            break
                        }
                    }
                    if (!mapped) {
                        mappedRanges.add(start until start + 1)
                        start++
                    }
                }
            }
            currentRanges = mappedRanges
        }

        for (locationRange in currentRanges) {
            minLocation = minOf(minLocation, locationRange.first)
        }
    }

    return minLocation
}

fun main() {
    val (seedRanges, maps) = parseInput("input.txt")

    val mapOrder = listOf(
        "seed-to-soil",
        "soil-to-fertilizer",
        "fertilizer-to-water",
        "water-to-light",
        "light-to-temperature",
        "temperature-to-humidity",
        "humidity-to-location"
    )

    val minLocation = mapRange(seedRanges, maps, mapOrder)
    println(minLocation)
}
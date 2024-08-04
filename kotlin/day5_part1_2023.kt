import java.io.File

data class Range(val destinationStart: Long, val sourceStart: Long, val length: Long)

fun main() {
    val input = File("input.txt").readText()
    val parts = input.split("\n\n")

    val seeds = parts[0].split(": ")[1].split(" ").map { it.toLong() }
    val maps = parts.drop(1).map { part ->
        part.split("\n").drop(1).map { line ->
            val (destStart, sourceStart, length) = line.split(" ").map { it.toLong() }
            Range(destStart, sourceStart, length)
        }
    }

    val locations = seeds.map { seed ->
        var value = seed
        for (map in maps) {
            value = map.firstOrNull { range -> value in range.sourceStart until range.sourceStart + range.length }
                ?.let { range -> range.destinationStart + (value - range.sourceStart) }
                ?: value
        }
        value
    }

    println(locations.minOrNull())
}
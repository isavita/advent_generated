
import java.io.File

data class Row(val springs: String, val group: List<Int>)

fun parseInput(input: List<String>): List<Row> =
    input.map { line ->
        val (springs, groups) = line.split(" ")
        Row(springs, groups.split(",").map { it.toInt() })
    }

fun countArrangementsRecursive(
    row: Row,
    iSprings: Int,
    iGroup: Int,
    iContiguousDamaged: Int,
    cache: MutableMap<Triple<Int, Int, Int>, Long>
): Long {
    if (iSprings == row.springs.length) {
        return if (iGroup == row.group.size && iContiguousDamaged == 0) 1
        else if (iGroup == row.group.size - 1 && iContiguousDamaged == row.group[iGroup]) 1
        else 0
    }

    val cacheKey = Triple(iSprings, iGroup, iContiguousDamaged)
    cache[cacheKey]?.let { return it }

    var res = 0L
    val char = row.springs[iSprings]
    if (char == '.' || char == '?') {
        if (iContiguousDamaged == 0) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache)
        } else if (iContiguousDamaged == row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache)
        }
    }
    if (char == '#' || char == '?') {
        if (iGroup < row.group.size && iContiguousDamaged < row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache)
        }
    }

    cache[cacheKey] = res
    return res
}

fun countArrangements(row: Row): Long =
    countArrangementsRecursive(row, 0, 0, 0, mutableMapOf())

fun unfoldRow(row: Row, unfoldingFactor: Int): Row {
    val newSprings = generateSequence { row.springs }.take(unfoldingFactor).joinToString("?")
    val newGroups = List(unfoldingFactor) { row.group }.flatten()
    return Row(newSprings, newGroups)
}

fun solve(input: List<String>): Long {
    val rows = parseInput(input)
    val unfoldedRows = rows.map { unfoldRow(it, 5) }
    return unfoldedRows.sumOf { countArrangements(it) }
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}

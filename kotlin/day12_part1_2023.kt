import java.io.File

data class Row(val springs: String, val group: List<Int>)

fun parseInput(input: List<String>): List<Row> {
    val rows = mutableListOf<Row>()
    for (line in input) {
        val parts = line.split(" ")
        val springs = parts[0]
        val ints = parseStringToInts(parts[1])

        val row = Row(springs, ints)
        rows.add(row)
    }
    return rows
}

fun parseStringToInts(numbersLine: String): List<Int> {
    val numbers = mutableListOf<Int>()
    val numbersParts = numbersLine.split(",")
    for (numberStr in numbersParts) {
        val number = numberStr.toInt()
        numbers.add(number)
    }
    return numbers
}

fun countArrangementsRecursive(row: Row, iSprings: Int, iGroup: Int, iContiguousDamaged: Int, cache: MutableMap<Triple<Int, Int, Int>, Int>): Int {
    if (iSprings == row.springs.length) {
        if (iGroup == row.group.size && iContiguousDamaged == 0) {
            return 1
        } else if (iGroup == row.group.size - 1 && iContiguousDamaged == row.group[iGroup]) {
            return 1
        }
        return 0
    }

    val cacheKey = Triple(iSprings, iGroup, iContiguousDamaged)
    if (cache.containsKey(cacheKey)) {
        return cache.getValue(cacheKey)
    }

    var res = 0
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

fun countArrangements(row: Row): Int {
    return countArrangementsRecursive(row, 0, 0, 0, mutableMapOf())
}

fun unfoldRow(row: Row, unfoldingFactor: Int): Row {
    var newRow = Row(row.springs, row.group)

    for (i in 1 until unfoldingFactor) {
        newRow = Row("?${newRow.springs}", newRow.group + row.group)
    }

    return newRow
}

fun solve(input: List<String>): Int {
    val rows = parseInput(input)

    var res = 0
    for (row in rows) {
        res += countArrangements(row)
    }

    return res
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}
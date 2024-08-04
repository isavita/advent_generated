import java.io.File

data class Galaxy(val x: Int, val y: Int)

fun main() {
    val input = File("input.txt").readLines()
    val galaxies = mutableListOf<Galaxy>()

    // Identify galaxies
    for ((y, line) in input.withIndex()) {
        for ((x, char) in line.withIndex()) {
            if (char == '#') {
                galaxies.add(Galaxy(x, y))
            }
        }
    }

    // Identify empty rows and columns
    val emptyRows = mutableListOf<Int>()
    val emptyCols = mutableListOf<Int>()

    for (i in input.indices) {
        if (input[i].all { it == '.' }) {
            emptyRows.add(i)
        }
    }

    for (i in input[0].indices) {
        if (input.all { it[i] == '.' }) {
            emptyCols.add(i)
        }
    }

    // Calculate the sum of shortest paths
    var totalDistance = 0L
    val expansionFactor = 1_000_000

    for (i in 0 until galaxies.size) {
        for (j in i + 1 until galaxies.size) {
            val (x1, y1) = galaxies[i]
            val (x2, y2) = galaxies[j]

            val minX = minOf(x1, x2)
            val maxX = maxOf(x1, x2)
            val minY = minOf(y1, y2)
            val maxY = maxOf(y1, y2)

            val expandedRows = emptyRows.count { it in minY..maxY }
            val expandedCols = emptyCols.count { it in minX..maxX }

            val distance = (maxX - minX) + (maxY - minY) + (expandedRows + expandedCols) * (expansionFactor - 1)
            totalDistance += distance
        }
    }

    println(totalDistance)
}
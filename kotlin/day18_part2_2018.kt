
import java.io.File

private const val OPEN = '.'
private const val TREES = '|'
private const val LUMBERYARD = '#'
private const val SIZE = 50

fun main() {
    val grid = readInput("input.txt")
    val seenStates = mutableMapOf<String, Int>()
    var cycleStart = 0
    var cycleLength = 0

    var currentGrid = grid
    for (minute in 0..Int.MAX_VALUE) {
        val state = gridToString(currentGrid)
        if (seenStates.containsKey(state)) {
            cycleStart = seenStates[state]!!
            cycleLength = minute - cycleStart
            break
        }
        seenStates[state] = minute
        currentGrid = transform(currentGrid)
    }

    val remainingMinutes = (1000000000 - cycleStart) % cycleLength
    for (i in 0 until remainingMinutes) {
        currentGrid = transform(currentGrid)
    }

    val (wooded, lumberyards) = countResources(currentGrid)
    println(wooded * lumberyards)
}

private fun readInput(filename: String): Array<CharArray> {
    val file = File(filename)
    return file.readLines().map { it.toCharArray() }.toTypedArray()
}

private fun transform(grid: Array<CharArray>): Array<CharArray> {
    val newGrid = Array(grid.size) { CharArray(grid[0].size) }
    for (i in grid.indices) {
        for (j in grid[i].indices) {
            newGrid[i][j] = nextAcreState(grid, i, j)
        }
    }
    return newGrid
}

private fun nextAcreState(grid: Array<CharArray>, i: Int, j: Int): Char {
    return when (grid[i][j]) {
        OPEN -> if (countAdjacent(grid, i, j, TREES) >= 3) TREES else OPEN
        TREES -> if (countAdjacent(grid, i, j, LUMBERYARD) >= 3) LUMBERYARD else TREES
        LUMBERYARD -> if (countAdjacent(grid, i, j, LUMBERYARD) >= 1 && countAdjacent(grid, i, j, TREES) >= 1) LUMBERYARD else OPEN
        else -> grid[i][j]
    }
}

private fun countAdjacent(grid: Array<CharArray>, i: Int, j: Int, acreType: Char): Int {
    var count = 0
    for (x in -1..1) {
        for (y in -1..1) {
            if (x == 0 && y == 0) continue
            val ni = i + x
            val nj = j + y
            if (ni in grid.indices && nj in grid[0].indices && grid[ni][nj] == acreType) {
                count++
            }
        }
    }
    return count
}

private fun countResources(grid: Array<CharArray>): Pair<Int, Int> {
    var wooded = 0
    var lumberyards = 0
    for (row in grid) {
        for (acre in row) {
            when (acre) {
                TREES -> wooded++
                LUMBERYARD -> lumberyards++
            }
        }
    }
    return Pair(wooded, lumberyards)
}

private fun gridToString(grid: Array<CharArray>): String {
    return grid.joinToString("\n") { it.joinToString("") }
}

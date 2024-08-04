import java.io.File
import kotlin.math.max
import kotlin.math.min

fun main() {
    val input = File("input.txt").readText().trim()
    println(solve(input))
}

fun solve(input: String): Int {
    val matrix = parseInput(input)
    var originCol = 0
    for (i in matrix[0].indices) {
        if (matrix[0][i] == "+") {
            originCol = i
        }
        matrix[matrix.size - 1][i] = "#"
    }

    var ans = 0
    while (!dropSand(matrix, originCol)) {
        ans++
        if (matrix[0][originCol] == "o") {
            break
        }
    }

    return ans
}

fun parseInput(input: String): Array<Array<String>> {
    val coordSets = mutableListOf<Array<IntArray>>()
    var lowestCol = Int.MAX_VALUE
    var highestRow = 0
    for (line in input.split("\n")) {
        val rawCoords = line.split(" -> ")
        val coords = mutableListOf<IntArray>()
        for (rawCoord in rawCoords) {
            val rawNums = rawCoord.split(",")
            val col = rawNums[0].toInt()
            val row = rawNums[1].toInt()
            val coord = intArrayOf(col, row)
            coords.add(coord)

            lowestCol = min(lowestCol, col)
            highestRow = max(highestRow, row)
        }
        coordSets.add(coords.toTypedArray())
    }

    val ExtraLeftSpace = 200

    var highestCol = 0
    for (set in coordSets) {
        for (i in set.indices) {
            set[i][0] -= lowestCol - ExtraLeftSpace
            highestCol = max(highestCol, set[i][0])
        }
    }

    val matrix = Array(highestRow + 3) { Array(highestCol + ExtraLeftSpace * 2) { "." } }

    for (set in coordSets) {
        for (i in 1 until set.size) {
            val cols = intArrayOf(set[i - 1][0], set[i][0]).sorted()
            val rows = intArrayOf(set[i - 1][1], set[i][1]).sorted()

            if (cols[0] == cols[1]) {
                for (r in rows[0]..rows[1]) {
                    matrix[r][cols[0]] = "#"
                }
            } else if (rows[0] == rows[1]) {
                for (c in cols[0]..cols[1]) {
                    matrix[rows[0]][c] = "#"
                }
            }
        }
    }

    val originCol = 500 - lowestCol + ExtraLeftSpace
    matrix[0][originCol] = "+"

    return matrix
}

fun dropSand(matrix: Array<Array<String>>, originCol: Int): Boolean {
    var r = 0
    var c = originCol

    while (r < matrix.size - 1) {
        val below = matrix[r + 1][c]
        val diagonallyLeft = matrix[r + 1][c - 1]
        val diagonallyRight = matrix[r + 1][c + 1]
        if (below == ".") {
            r++
        } else if (diagonallyLeft == ".") {
            r++
            c--
        } else if (diagonallyRight == ".") {
            r++
            c++
        } else {
            matrix[r][c] = "o"
            return false
        }
    }

    return true
}
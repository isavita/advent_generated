
import java.io.File
import kotlin.math.sqrt

fun main() {
    val input = File("input.txt").readText().trim()
    val ans = solve(input)
    println(ans)
}

fun solve(input: String): Long {
    val tiles = parseTilesFromInput(input)
    val edgeSize = sqrt(tiles.size.toDouble()).toInt()
    val assembledTiles = backtrackAssemble(tiles, edgeSize)
    return assembledTiles[0][0].id.toLong() *
            assembledTiles[0][edgeSize - 1].id.toLong() *
            assembledTiles[edgeSize - 1][0].id.toLong() *
            assembledTiles[edgeSize - 1][edgeSize - 1].id.toLong()
}

data class Tile(val contents: List<List<String>>, val id: Int)

fun parseTilesFromInput(input: String): List<Tile> {
    return input.split("\n\n").map { block ->
        val lines = block.lines()
        val tileId = lines[0].substringAfter("Tile ").substringBefore(":").toInt()
        val contents = lines.drop(1).map { it.split("") }.map { it.filter { it.isNotBlank() } }
        Tile(contents, tileId)
    }
}

fun backtrackAssemble(tiles: List<Tile>, edgeSize: Int): Array<Array<Tile>> {
    val assembledTiles = Array(edgeSize) { arrayOfNulls<Tile>(edgeSize) }
    val usedIndices = BooleanArray(tiles.size)
    
    fun recurse(row: Int, col: Int): Boolean {
        if (row == edgeSize) return true
        val nextRow = if (col == edgeSize - 1) row + 1 else row
        val nextCol = if (col == edgeSize - 1) 0 else col + 1

        if (assembledTiles[row][col] != null) return recurse(nextRow, nextCol)

        for (i in tiles.indices) {
            if (!usedIndices[i]) {
                val tile = tiles[i]
                for (orientation in allGridOrientations(tile.contents)) {
                    if (row != 0) {
                        val currentTopRow = getRow(orientation, true)
                        val bottomOfAbove = getRow(assembledTiles[row - 1][col]!!.contents, false)
                        if (currentTopRow != bottomOfAbove) continue
                    }
                    if (col != 0) {
                        val currentLeftCol = getCol(orientation, true)
                        val rightColOfLeft = getCol(assembledTiles[row][col - 1]!!.contents, false)
                        if (currentLeftCol != rightColOfLeft) continue
                    }

                    assembledTiles[row][col] = Tile(orientation, tile.id)
                    usedIndices[i] = true
                    if (recurse(nextRow, nextCol)) return true
                    assembledTiles[row][col] = null
                    usedIndices[i] = false
                }
            }
        }
        return false
    }

    recurse(0, 0)
    return assembledTiles.map { it.requireNoNulls() }.toTypedArray()
}

fun getCol(grid: List<List<String>>, firstCol: Boolean): String {
    return grid.joinToString("") { if (firstCol) it[0] else it.last() }
}

fun getRow(grid: List<List<String>>, firstRow: Boolean): String {
    return if (firstRow) grid[0].joinToString("") else grid.last().joinToString("")
}

fun allGridOrientations(grid: List<List<String>>): List<List<List<String>>> {
    val orientations = mutableListOf(grid)
    for (i in 0 until 3) {
        orientations.add(rotateStringGrid(orientations.last()))
    }
    for (i in 0 until 4) {
        orientations.add(mirrorStringGrid(orientations[i]))
    }
    return orientations
}

fun rotateStringGrid(grid: List<List<String>>): List<List<String>> {
    val rows = grid.size
    val cols = grid[0].size
    return List(cols) { r ->
        List(rows) { c ->
            grid[rows - 1 - c][r]
        }
    }
}

fun mirrorStringGrid(grid: List<List<String>>): List<List<String>> {
    return grid.map { it.reversed() }
}

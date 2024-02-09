import java.io.File

data class Coord(val x: Int, val y: Int)

data class Grid(val width: Int, val height: Int, val data: MutableMap<Coord, Char>)

val empty: Char = '.'

fun buildGrid(input: List<String>, empty: Char): Grid {
    val grid = Grid(
        width = input[0].length,
        height = input.size,
        data = mutableMapOf()
    )

    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char != empty) {
                grid.data[Coord(x, y)] = char
            }
        }
    }

    return grid
}

fun Grid.toString(empty: Char): String {
    val result = StringBuilder()

    for (y in 0 until height) {
        for (x in 0 until width) {
            val coord = Coord(x, y)
            result.append(data[coord] ?: empty)
        }
        result.append('\n')
    }

    return result.toString()
}

fun Grid.getEmptyRows(): List<Int> {
    val emptyRows = mutableListOf<Int>()
    for (y in 0 until height) {
        var isEmpty = true

        var x = 0
        while (x < width) {
            if (data.containsKey(Coord(x, y))) {
                isEmpty = false
            }
            x++
        }

        if (isEmpty) {
            emptyRows.add(y)
        }
    }
    return emptyRows
}

fun Grid.getEmptyCols(): List<Int> {
    val emptyCols = mutableListOf<Int>()
    for (x in 0 until width) {
        var isEmpty = true

        var y = 0
        while (y < height) {
            if (data.containsKey(Coord(x, y))) {
                isEmpty = false
            }
            y++
        }

        if (isEmpty) {
            emptyCols.add(x)
        }
    }
    return emptyCols
}

fun calculateOffsets(emptyIndexes: List<Int>, bound: Int): List<Int> {
    val offsets = MutableList(bound) { 0 }
    emptyIndexes.forEach { idx ->
        for (i in idx + 1 until offsets.size) {
            offsets[i]++
        }
    }
    return offsets
}

fun expandGrid(grid: Grid, expansionFactor: Int): Grid {
    val emptyCols = grid.getEmptyCols()
    val emptyRows = grid.getEmptyRows()
    val numLinesToAdd = expansionFactor - 1

    val newGrid = Grid(
        width = grid.width + emptyCols.size * numLinesToAdd,
        height = grid.height + emptyRows.size * numLinesToAdd,
        data = mutableMapOf()
    )

    val dXs = calculateOffsets(emptyCols, grid.width)
    val dYs = calculateOffsets(emptyRows, grid.height)

    for (y in 0 until grid.height) {
        for (x in 0 until grid.width) {
            val coord = Coord(x, y)
            grid.data[coord]?.let { char ->
                val newCoord = Coord(x + dXs[x] * numLinesToAdd, y + dYs[y] * numLinesToAdd)
                newGrid.data[newCoord] = char
            }
        }
    }

    return newGrid
}

fun abs(x: Int): Int = if (x < 0) -x else x

fun calculateLength(grid: Grid, c1: Coord, c2: Coord): Int {
    val dX = abs(c2.x - c1.x)
    val dY = abs(c2.y - c1.y)
    return dX + dY
}

fun solve(input: List<String>): Int {
    val grid = buildGrid(input, empty)

    val expandedGrid = expandGrid(grid, 2)

    var res = 0
    val alreadySeen = mutableSetOf<Coord>()
    expandedGrid.data.keys.forEach { coord1 ->
        alreadySeen.forEach { coord2 ->
            val length = calculateLength(expandedGrid, coord1, coord2)
            res += length
        }
        alreadySeen.add(coord1)
    }

    return res
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}
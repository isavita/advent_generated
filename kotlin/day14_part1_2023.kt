import java.io.File

data class Coord(val x: Int, val y: Int) {
    fun add(c: Coord) = Coord(x + c.x, y + c.y)
    fun isInBounds(width: Int, height: Int) = x in 0 until width && y in 0 until height
}

data class Grid(val width: Int, val height: Int, val data: MutableMap<Coord, Char>)

val empty = '.'
val cubicRock = '#'
val roundRock = 'O'

val north = Coord(0, -1)
val west = Coord(-1, 0)
val south = Coord(0, 1)
val east = Coord(1, 0)

fun buildGrid(input: List<String>): Grid {
    val grid = Grid(input[0].length, input.size, mutableMapOf())

    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char != empty) {
                grid.data[Coord(x, y)] = char
            }
        }
    }

    return grid
}

fun Grid.toString(): String {
    val result = StringBuilder()

    for (y in 0 until height) {
        for (x in 0 until width) {
            val coord = Coord(x, y)
            result.append(data[coord] ?: empty)
        }
        result.append("\n")
    }

    return result.toString()
}

fun shiftSingleRock(grid: Grid, coord: Coord, dir: Coord) {
    if (grid.data[coord] == roundRock) {
        var current = coord
        var before = coord.add(dir)

        while (!grid.data.containsKey(before) && before.isInBounds(grid.width, grid.height)) {
            grid.data[before] = roundRock
            grid.data.remove(current)

            current = before
            before = before.add(dir)
        }
    }
}

fun shiftRocks(grid: Grid, dir: Coord) {
    when (dir) {
        north, west -> {
            for (x in 0 until grid.width) {
                for (y in 0 until grid.height) {
                    shiftSingleRock(grid, Coord(x, y), dir)
                }
            }
        }
        south, east -> {
            for (x in grid.width - 1 downTo 0) {
                for (y in grid.height - 1 downTo 0) {
                    shiftSingleRock(grid, Coord(x, y), dir)
                }
            }
        }
    }
}

fun calculateLoad(grid: Grid): Int {
    var load = 0

    for (x in 0 until grid.width) {
        for (y in 0 until grid.height) {
            val coord = Coord(x, y)
            if (grid.data[coord] == roundRock) {
                load += grid.height - y
            }
        }
    }

    return load
}

fun solve(input: List<String>): Int {
    val grid = buildGrid(input)
    shiftRocks(grid, north)

    return calculateLoad(grid)
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}
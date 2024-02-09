import java.io.File

data class Coord(val x: Int, val y: Int) {
    fun add(c: Coord) = Coord(x + c.x, y + c.y)
}

data class Grid(val width: Int, val height: Int, val data: MutableMap<Coord, Char>)

fun Coord.isInBounds(grid: Grid) = x in 0 until grid.width && y in 0 until grid.height

const val Empty = '.'
const val CubicRock = '#'
const val RoundRock = 'O'

val North = Coord(0, -1)
val West = Coord(-1, 0)
val South = Coord(0, 1)
val East = Coord(1, 0)

fun buildGrid(input: List<String>): Grid {
    val grid = Grid(input[0].length, input.size, mutableMapOf())

    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char != Empty) {
                grid.data[Coord(x, y)] = char
            }
        }
    }

    return grid
}

fun Grid.toStringGrid(): String {
    var result = ""

    repeat(height) { y ->
        repeat(width) { x ->
            val coord = Coord(x, y)
            result += data[coord] ?: Empty
        }
        result += "\n"
    }

    return result
}

fun shiftSingleRock(grid: Grid, coord: Coord, dir: Coord) {
    if (grid.data[coord] == RoundRock) {
        var current = coord
        var before = coord.add(dir)

        while (!grid.data.containsKey(before) && before.isInBounds(grid)) {
            grid.data[before] = RoundRock
            grid.data.remove(current)

            current = before
            before = before.add(dir)
        }
    }
}

fun shiftRocks(grid: Grid, dir: Coord) {
    when (dir) {
        North, West -> {
            repeat(grid.width) { x ->
                repeat(grid.height) { y ->
                    shiftSingleRock(grid, Coord(x, y), dir)
                }
            }
        }
        South, East -> {
            for (x in grid.width - 1 downTo 0) {
                for (y in grid.height - 1 downTo 0) {
                    shiftSingleRock(grid, Coord(x, y), dir)
                }
            }
        }
    }
}

fun cycleRocks(grid: Grid) {
    shiftRocks(grid, North)
    shiftRocks(grid, West)
    shiftRocks(grid, South)
    shiftRocks(grid, East)
}

fun calculateGridKey(grid: Grid): Int {
    var key = 0

    repeat(grid.width) { x ->
        repeat(grid.height) { y ->
            val coord = Coord(x, y)
            if (grid.data[coord] == RoundRock) {
                key += coord.x + coord.y * grid.width
            }
        }
    }

    return key
}

fun calculateLoad(grid: Grid): Int {
    var load = 0

    repeat(grid.width) { x ->
        repeat(grid.height) { y ->
            val coord = Coord(x, y)
            if (grid.data[coord] == RoundRock) {
                load += grid.height - y
            }
        }
    }

    return load
}

fun solve(input: List<String>): Int {
    val numCycles = 1000000000
    val grid = buildGrid(input)
    val cache = mutableMapOf<Int, Int>()

    repeat(numCycles) { i ->
        val gridKey = calculateGridKey(grid)
        cache[gridKey]?.let { iStartCycle ->
            val remainingCycles = (numCycles - iStartCycle) % (i - iStartCycle)
            repeat(remainingCycles) {
                cycleRocks(grid)
            }
            return calculateLoad(grid)
        }
        cache[gridKey] = i

        cycleRocks(grid)
    }

    return calculateLoad(grid)
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}
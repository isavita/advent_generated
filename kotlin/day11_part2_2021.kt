import java.io.File

fun main(args: Array<String>) {
    val grid = readInput("input.txt")
    var step = 0

    while (true) {
        step++
        val flashes = simulateStep(grid)
        if (flashes == 100) {
            break
        }
    }

    println(step)
}

fun readInput(filename: String): Array<IntArray> {
    val file = File(filename)
    val grid = mutableListOf<IntArray>()

    file.forEachLine {
        val row = IntArray(it.length) { i -> it[i].toString().toInt() }
        grid.add(row)
    }

    return grid.toTypedArray()
}

fun simulateStep(grid: Array<IntArray>): Int {
    var flashes = 0
    val flashed = mutableSetOf<Pair<Int, Int>>()

    for (y in grid.indices) {
        for (x in grid[y].indices) {
            grid[y][x]++
        }
    }

    for (y in grid.indices) {
        for (x in grid[y].indices) {
            if (grid[y][x] > 9) {
                flashes += flash(grid, x, y, flashed)
            }
        }
    }

    flashed.forEach { coord ->
        grid[coord.second][coord.first] = 0
    }

    return flashes
}

fun flash(grid: Array<IntArray>, x: Int, y: Int, flashed: MutableSet<Pair<Int, Int>>): Int {
    if (Pair(x, y) in flashed) {
        return 0
    }

    flashed.add(Pair(x, y))
    var flashes = 1
    val directions = arrayOf(-1 to -1, -1 to 0, -1 to 1, 0 to -1, 0 to 1, 1 to -1, 1 to 0, 1 to 1)

    directions.forEach { dir ->
        val newX = x + dir.first
        val newY = y + dir.second
        if (newX >= 0 && newX < grid[0].size && newY >= 0 && newY < grid.size) {
            grid[newY][newX]++
            if (grid[newY][newX] > 9) {
                flashes += flash(grid, newX, newY, flashed)
            }
        }
    }

    return flashes
}
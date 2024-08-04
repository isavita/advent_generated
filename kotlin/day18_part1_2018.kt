import java.io.File

data class Point(val x: Int, val y: Int)

fun readInput(fileName: String): List<String> {
    return File(fileName).readLines()
}

fun getNeighbors(point: Point, maxX: Int, maxY: Int): List<Point> {
    val neighbors = mutableListOf<Point>()
    for (dx in -1..1) {
        for (dy in -1..1) {
            if (dx == 0 && dy == 0) continue
            val newX = point.x + dx
            val newY = point.y + dy
            if (newX in 0 until maxX && newY in 0 until maxY) {
                neighbors.add(Point(newX, newY))
            }
        }
    }
    return neighbors
}

fun simulateMinute(grid: List<String>): List<String> {
    val newGrid = grid.map { it.toCharArray() }.toMutableList()
    val maxX = grid.size
    val maxY = grid[0].length

    for (x in 0 until maxX) {
        for (y in 0 until maxY) {
            val current = grid[x][y]
            val neighbors = getNeighbors(Point(x, y), maxX, maxY)
            val trees = neighbors.count { grid[it.x][it.y] == '|' }
            val lumberyards = neighbors.count { grid[it.x][it.y] == '#' }

            when (current) {
                '.' -> if (trees >= 3) newGrid[x][y] = '|'
                '|' -> if (lumberyards >= 3) newGrid[x][y] = '#'
                '#' -> if (lumberyards >= 1 && trees >= 1) newGrid[x][y] = '#' else newGrid[x][y] = '.'
            }
        }
    }

    return newGrid.map { String(it) }
}

fun calculateResourceValue(grid: List<String>): Int {
    var trees = 0
    var lumberyards = 0

    for (row in grid) {
        for (cell in row) {
            when (cell) {
                '|' -> trees++
                '#' -> lumberyards++
            }
        }
    }

    return trees * lumberyards
}

fun main() {
    val input = readInput("input.txt")
    var grid = input

    for (minute in 1..10) {
        grid = simulateMinute(grid)
    }

    val resourceValue = calculateResourceValue(grid)
    println("Total resource value after 10 minutes: $resourceValue")
}
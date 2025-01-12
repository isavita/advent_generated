
import java.io.File

const val gridSize = 100
const val steps = 100

fun countOnNeighbors(grid: BooleanArray, x: Int, y: Int): Int {
    var on = 0
    for (dx in -1..1) {
        for (dy in -1..1) {
            if (dx == 0 && dy == 0) continue
            val nx = x + dx
            val ny = y + dy
            if (nx in 0 until gridSize && ny in 0 until gridSize && grid[nx * gridSize + ny]) {
                on++
            }
        }
    }
    return on
}

fun step(grid: BooleanArray): BooleanArray {
    val newGrid = BooleanArray(gridSize * gridSize)
    for (x in 0 until gridSize) {
        for (y in 0 until gridSize) {
            val onNeighbors = countOnNeighbors(grid, x, y)
            val index = x * gridSize + y
            newGrid[index] = if (grid[index]) {
                onNeighbors == 2 || onNeighbors == 3
            } else {
                onNeighbors == 3
            }
        }
    }
    newGrid[0] = true
    newGrid[gridSize - 1] = true
    newGrid[gridSize * (gridSize - 1)] = true
    newGrid[gridSize * gridSize - 1] = true
    return newGrid
}

fun main() {
    val grid = File("input.txt").readLines().flatMapIndexed { y, line ->
        line.mapIndexed { x, c ->
            x * gridSize + y to (c == '#')
        }
    }.toMap().let {
        BooleanArray(gridSize * gridSize) { index ->
            it[index] ?: false
        }
    }

    grid[0] = true
    grid[gridSize - 1] = true
    grid[gridSize * (gridSize - 1)] = true
    grid[gridSize * gridSize - 1] = true

    var currentGrid = grid
    repeat(steps) {
        currentGrid = step(currentGrid)
    }

    val onCount = currentGrid.count { it }
    println(onCount)
}

import java.io.File

fun main() {
    val initialGrid = readInput("input.txt")
    var currentGrid = initialGrid

    for (step in 1..100) {
        currentGrid = animateGrid(currentGrid)
    }

    val lightsOn = countLightsOn(currentGrid)
    println("Number of lights on after 100 steps: $lightsOn")
}

fun readInput(fileName: String): List<String> {
    val file = File(fileName)
    return file.readLines()
}

fun animateGrid(grid: List<String>): List<String> {
    val size = grid.size
    val newGrid = MutableList(size) { "" }

    for (i in 0 until size) {
        for (j in 0 until size) {
            val neighborsOn = countNeighborsOn(grid, i, j)
            val currentState = grid[i][j]
            val newState = if (currentState == '#') {
                if (neighborsOn == 2 || neighborsOn == 3) '#' else '.'
            } else {
                if (neighborsOn == 3) '#' else '.'
            }
            newGrid[i] += newState
        }
    }

    return newGrid
}

fun countNeighborsOn(grid: List<String>, x: Int, y: Int): Int {
    val size = grid.size
    val directions = listOf(
        -1 to -1, -1 to 0, -1 to 1,
        0 to -1,         0 to 1,
        1 to -1,  1 to 0,  1 to 1
    )

    var count = 0
    for ((dx, dy) in directions) {
        val nx = x + dx
        val ny = y + dy
        if (nx in 0 until size && ny in 0 until size && grid[nx][ny] == '#') {
            count++
        }
    }

    return count
}

fun countLightsOn(grid: List<String>): Int {
    return grid.sumOf { row -> row.count { it == '#' } }
}
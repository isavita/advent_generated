import java.io.File

fun main() {
    // Read input from file
    val input = File("input.txt").readLines()
    val grid = input.map { line -> line.map { it.digitToInt() }.toMutableList() }.toMutableList()

    var totalFlashes = 0

    // Simulate 100 steps
    repeat(100) {
        totalFlashes += simulateStep(grid)
    }

    // Print the total number of flashes
    println("Total flashes after 100 steps: $totalFlashes")
}

fun simulateStep(grid: MutableList<MutableList<Int>>): Int {
    val flashed = mutableSetOf<Pair<Int, Int>>()
    val toFlash = mutableListOf<Pair<Int, Int>>()

    // Increase energy level of each octopus by 1
    for (i in grid.indices) {
        for (j in grid[i].indices) {
            grid[i][j]++
            if (grid[i][j] > 9) {
                toFlash.add(i to j)
            }
        }
    }

    // Process flashes
    while (toFlash.isNotEmpty()) {
        val (x, y) = toFlash.removeFirst()
        if ((x to y) in flashed) continue

        flashed.add(x to y)

        // Increase energy level of all adjacent octopuses
        for (dx in -1..1) {
            for (dy in -1..1) {
                if (dx == 0 && dy == 0) continue
                val nx = x + dx
                val ny = y + dy
                if (nx in grid.indices && ny in grid[nx].indices) {
                    grid[nx][ny]++
                    if (grid[nx][ny] > 9 && (nx to ny) !in flashed) {
                        toFlash.add(nx to ny)
                    }
                }
            }
        }
    }

    // Reset flashed octopuses to 0
    for ((x, y) in flashed) {
        grid[x][y] = 0
    }

    return flashed.size
}
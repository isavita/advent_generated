import java.io.File

fun main(args: Array<String>) {
    val grid = mutableMapOf<Pair<Int, Int>, Int>() // Clean = 0, Weakened = 1, Infected = 2, Flagged = 3
    var startX = 0
    var startY = 0

    File("input.txt").forEachLine { line ->
        line.forEachIndexed { x, c ->
            if (c == '#') {
                grid[Pair(x, startY)] = 2
            }
        }
        startX = line.length / 2
        startY++
    }

    val dx = listOf(0, 1, 0, -1)
    val dy = listOf(-1, 0, 1, 0)

    var x = startX
    var y = startY / 2
    var dir = 0 // Start facing up
    var infectedCount = 0

    repeat(10000000) {
        val pos = Pair(x, y)
        when (grid.getOrDefault(pos, 0)) {
            0 -> {
                dir = (dir - 1 + 4) % 4
                grid[pos] = 1
            }
            1 -> {
                grid[pos] = 2
                infectedCount++
            }
            2 -> {
                dir = (dir + 1) % 4
                grid[pos] = 3
            }
            3 -> {
                dir = (dir + 2) % 4
                grid[pos] = 0
            }
        }
        x += dx[dir]
        y += dy[dir]
    }

    println(infectedCount)
}
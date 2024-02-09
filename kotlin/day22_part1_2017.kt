import java.io.File

data class Position(val x: Int, val y: Int)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    val grid = mutableMapOf<Position, Boolean>()
    var startX = 0
    var startY = 0

    lines.forEachIndexed { y, line ->
        line.forEachIndexed { x, c ->
            if (c == '#') {
                grid[Position(x, y)] = true
            }
        }
        startX = line.length / 2
        startY = y / 2
    }

    val dx = listOf(0, 1, 0, -1)
    val dy = listOf(-1, 0, 1, 0)

    var x = startX
    var y = startY
    var dir = 0 // Start facing up
    var infectedCount = 0

    repeat(10000) {
        val pos = Position(x, y)
        if (grid[pos] == true) {
            dir = (dir + 1) % 4
            grid.remove(pos)
        } else {
            dir = (dir - 1 + 4) % 4
            grid[pos] = true
            infectedCount++
        }
        x += dx[dir]
        y += dy[dir]
    }

    println(infectedCount)
}
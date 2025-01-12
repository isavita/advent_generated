
import java.io.File
import java.util.*

fun main() {
    val input = File("input.txt").readLines()
    val bytes = input.map { it.split(",").map { it.toInt() } }

    val gridSize = 71
    val start = Pair(0, 0)
    val end = Pair(gridSize - 1, gridSize - 1)

    fun solvePart1(): Int {
        val grid = Array(gridSize) { BooleanArray(gridSize) { false } }
        for (i in 0 until 1024) {
            val (x, y) = bytes[i]
            grid[y][x] = true
        }
        return bfs(grid, start, end)
    }

    fun solvePart2(): String {
        val grid = Array(gridSize) { BooleanArray(gridSize) { false } }
        for (i in bytes.indices) {
            val (x, y) = bytes[i]
            grid[y][x] = true
            if (bfs(grid, start, end) == -1) {
                return "$x,$y"
            }
        }
        return "No solution found"
    }

    println(solvePart1())
    println(solvePart2())
}

fun bfs(grid: Array<BooleanArray>, start: Pair<Int, Int>, end: Pair<Int, Int>): Int {
    val gridSize = grid.size
    val queue: Queue<Pair<Int, Int>> = LinkedList()
    val visited = Array(gridSize) { BooleanArray(gridSize) { false } }
    val dist = Array(gridSize) { IntArray(gridSize) { -1 } }

    queue.offer(start)
    visited[start.second][start.first] = true
    dist[start.second][start.first] = 0

    val dx = intArrayOf(0, 0, 1, -1)
    val dy = intArrayOf(1, -1, 0, 0)

    while (queue.isNotEmpty()) {
        val current = queue.poll()
        if (current == end) {
            return dist[end.second][end.first]
        }

        for (i in 0 until 4) {
            val nx = current.first + dx[i]
            val ny = current.second + dy[i]

            if (nx in 0 until gridSize && ny in 0 until gridSize && !grid[ny][nx] && !visited[ny][nx]) {
                queue.offer(Pair(nx, ny))
                visited[ny][nx] = true
                dist[ny][nx] = dist[current.second][current.first] + 1
            }
        }
    }
    return -1
}

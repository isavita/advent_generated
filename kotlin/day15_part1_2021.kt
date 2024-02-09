import java.io.File
import java.util.PriorityQueue

data class Position(val x: Int, val y: Int, val risk: Int)

fun dijkstra(grid: Array<IntArray>): Int {
    val pq = PriorityQueue<Position>(compareBy { it.risk })
    pq.offer(Position(0, 0, 0))

    val rows = grid.size
    val cols = grid[0].size
    val dist = Array(rows) { IntArray(cols) { Int.MAX_VALUE } }
    dist[0][0] = 0

    val directions = listOf(Position(1, 0, 0), Position(0, 1, 0), Position(-1, 0, 0), Position(0, -1, 0))

    while (pq.isNotEmpty()) {
        val curr = pq.poll()
        if (curr.x == rows - 1 && curr.y == cols - 1) {
            return curr.risk
        }
        for (d in directions) {
            val nx = curr.x + d.x
            val ny = curr.y + d.y
            if (nx in 0 until rows && ny in 0 until cols) {
                val nextRisk = curr.risk + grid[nx][ny]
                if (nextRisk < dist[nx][ny]) {
                    dist[nx][ny] = nextRisk
                    pq.offer(Position(nx, ny, nextRisk))
                }
            }
        }
    }
    return -1
}

fun main(args: Array<String>) {
    val grid = File("input.txt").readLines().map { it.map { ch -> ch - '0' }.toIntArray() }.toTypedArray()
    println(dijkstra(grid))
}
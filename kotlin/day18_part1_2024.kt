
import java.io.File
import java.util.*

fun main() {
    val size = 71
    val grid = Array(size) { BooleanArray(size) }
    File("input.txt").bufferedReader().useLines { lines ->
        lines.take(1024).forEach { line ->
            val parts = line.split(",")
            val x = parts[0].toInt()
            val y = parts[1].toInt()
            if (x in 0 until size && y in 0 until size) {
                grid[y][x] = true
            }
        }
    }

    val dirs = arrayOf(1 to 0, -1 to 0, 0 to 1, 0 to -1)
    val visited = Array(size) { BooleanArray(size) }

    val q: Queue<Pair<Pair<Int, Int>, Int>> = LinkedList()
    q.offer(Pair(0 to 0, 0))
    visited[0][0] = true

    while (q.isNotEmpty()) {
        val (current, steps) = q.poll()
        val (x, y) = current

        if (x == size - 1 && y == size - 1) {
            println(steps)
            return
        }

        for ((dx, dy) in dirs) {
            val nx = x + dx
            val ny = y + dy
            if (nx in 0 until size && ny in 0 until size && !grid[ny][nx] && !visited[ny][nx]) {
                visited[ny][nx] = true
                q.offer(Pair(nx to ny, steps + 1))
            }
        }
    }
    println("No path")
}

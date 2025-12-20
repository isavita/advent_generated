
import java.io.File

fun main() {
    val raw = File("input.txt").readLines().filter { it.isNotEmpty() }
    if (raw.isEmpty()) return
    val rows = raw.size
    val cols = raw[0].length
    val grid = raw.map { it.toCharArray() }
    val dx = intArrayOf(-1, -1, -1, 0, 0, 1, 1, 1)
    val dy = intArrayOf(-1, 0, 1, -1, 1, -1, 0, 1)
    var acc = 0
    for (y in 0 until rows) {
        for (x in 0 until cols) {
            if (grid[y][x] != '@') continue
            var cnt = 0
            for (d in 0 until 8) {
                val nx = x + dx[d]
                val ny = y + dy[d]
                if (nx in 0 until cols && ny in 0 until rows && grid[ny][nx] == '@') cnt++
            }
            if (cnt < 4) acc++
        }
    }
    println(acc)
}

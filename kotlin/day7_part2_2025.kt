
import java.io.File

fun main() {
    // read non‑empty lines from input.txt
    val rawLines = File("input.txt").readLines()
    val lines = rawLines.filter { it.isNotEmpty() }
    if (lines.isEmpty()) {
        println(0)
        return
    }

    val height = lines.size
    val width = lines[0].length
    val grid = lines.map { it.toCharArray() }

    // locate start point 'S'
    var startX = -1
    var startY = -1
    outer@ for (y in 0 until height) {
        for (x in 0 until width) {
            if (grid[y][x] == 'S') {
                startX = x
                startY = y
                break@outer
            }
        }
    }
    if (startX == -1) {
        System.err.println("Start point 'S' not found")
        return
    }

    // dynamic programming: column → number of ways reaching it
    var counts = mutableMapOf<Int, Long>()
    counts[startX] = 1L

    for (y in startY until height) {
        val next = mutableMapOf<Int, Long>()
        for ((x, cnt) in counts) {
            val isSplitter = x in 0 until width && grid[y][x] == '^'
            if (isSplitter) {
                for (k in arrayOf(x - 1, x + 1)) {
                    next[k] = (next[k] ?: 0L) + cnt
                }
            } else {
                next[x] = (next[x] ?: 0L) + cnt
            }
        }
        counts = next
    }

    var total = 0L
    for (v in counts.values) total += v
    println(total)
}

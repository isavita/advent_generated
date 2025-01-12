
import java.io.File
import java.util.*

fun main() {
    val lines = File("input.txt").readLines()
    val nr = lines.size
    val nc = lines[0].length
    val grid = Array(nr) { IntArray(nc) }
    for (i in 0 until nr) {
        for (j in 0 until nc) {
            grid[i][j] = lines[i][j] - '0'
        }
    }

    val dirs = arrayOf(1 to 0, -1 to 0, 0 to 1, 0 to -1)
    val trailheads = mutableListOf<Pair<Int, Int>>()
    for (r in 0 until nr) {
        for (c in 0 until nc) {
            if (grid[r][c] == 0) {
                trailheads.add(r to c)
            }
        }
    }

    var sumScores = 0
    for ((tr, tc) in trailheads) {
        val reached = mutableSetOf<Pair<Int, Int>>()
        val queue: Queue<Triple<Int, Int, Int>> = LinkedList()
        queue.offer(Triple(tr, tc, 0))
        val visited = mutableSetOf<Triple<Int, Int, Int>>()

        while (queue.isNotEmpty()) {
            val (cr, cc, ch) = queue.poll()
            if (ch == 9) {
                reached.add(cr to cc)
                continue
            }
            for ((dr, dc) in dirs) {
                val nr2 = cr + dr
                val nc2 = cc + dc
                if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue
                if (grid[nr2][nc2] == ch + 1) {
                    val key = Triple(nr2, nc2, ch + 1)
                    if (visited.add(key)) {
                        queue.offer(key)
                    }
                }
            }
        }
        sumScores += reached.size
    }
    println(sumScores)
}

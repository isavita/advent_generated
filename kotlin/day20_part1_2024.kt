
import java.io.File
import java.util.*

fun main() {
    val grid = File("input.txt").readLines().map { it.toCharArray() }.toTypedArray()
    val h = grid.size
    val w = grid[0].size

    var S: Pair<Int, Int> = Pair(0, 0)
    var E: Pair<Int, Int> = Pair(0, 0)

    for (r in 0 until h) {
        for (c in 0 until w) {
            if (grid[r][c] == 'S') {
                S = Pair(r, c)
            } else if (grid[r][c] == 'E') {
                E = Pair(r, c)
            }
        }
    }

    val trackCells = mutableListOf<Pair<Int, Int>>()
    val walls = Array(h) { BooleanArray(w) }
    for (r in 0 until h) {
        for (c in 0 until w) {
            if (grid[r][c] == '#') {
                walls[r][c] = true
            } else {
                trackCells.add(Pair(r, c))
            }
        }
    }

    val dirs = listOf(Pair(1, 0), Pair(-1, 0), Pair(0, 1), Pair(0, -1))

    fun bfs(start: Pair<Int, Int>, ignoreWalls: Boolean = false): Array<IntArray> {
        val dist = Array(h) { IntArray(w) { -1 } }
        dist[start.first][start.second] = 0
        val q = LinkedList<Pair<Int, Int>>()
        q.add(start)
        while (q.isNotEmpty()) {
            val (r, c) = q.removeFirst()
            for ((dr, dc) in dirs) {
                val nr = r + dr
                val nc = c + dc
                if (nr in 0 until h && nc in 0 until w) {
                    if (!ignoreWalls && walls[nr][nc]) {
                        continue
                    }
                    if (dist[nr][nc] == -1) {
                        dist[nr][nc] = dist[r][c] + 1
                        q.add(Pair(nr, nc))
                    }
                }
            }
        }
        return dist
    }

    val distFromS = bfs(S)
    val distFromE = bfs(E)

    if (distFromS[E.first][E.second] == -1) {
        println(0)
        return
    }

    val normalCost = distFromS[E.first][E.second]

    fun isTrack(r: Int, c: Int): Boolean {
        return r in 0 until h && c in 0 until w && !walls[r][c]
    }

    var possibleCheats = 0
    for (startPos in trackCells) {
        val sd = distFromS[startPos.first][startPos.second]
        if (sd == -1) {
            continue
        }

        for ((dr1, dc1) in dirs) {
            val m1r = startPos.first + dr1
            val m1c = startPos.second + dc1
            if (m1r !in 0 until h || m1c !in 0 until w) {
                continue
            }
            for ((dr2, dc2) in dirs) {
                val m2r = m1r + dr2
                val m2c = m1c + dc2
                if (m2r !in 0 until h || m2c !in 0 until w) {
                    continue
                }
                if (!isTrack(m2r, m2c)) {
                    continue
                }
                val ed = distFromE[m2r][m2c]
                if (ed == -1) {
                    continue
                }
                val newCost = sd + 2 + ed
                val saving = normalCost - newCost
                if (saving >= 100) {
                    possibleCheats++
                }
            }
        }
    }
    println(possibleCheats)
}

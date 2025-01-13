
import java.io.File
import java.util.LinkedList

data class Point(val x: Int, val y: Int)

fun main() {
    val grid = File("input.txt").readLines()
    val h = grid.size
    val w = grid[0].length
    var start = Point(0, 0)
    var end = Point(0, 0)
    val walls = Array(h) { BooleanArray(w) }
    val trackCells = mutableListOf<Point>()

    for (i in 0 until h) {
        for (j in 0 until w) {
            val ch = grid[i][j]
            if (ch == 'S') {
                start = Point(i, j)
            } else if (ch == 'E') {
                end = Point(i, j)
            }
            if (ch == '#') {
                walls[i][j] = true
            } else {
                trackCells.add(Point(i, j))
            }
        }
    }

    val dirs = listOf(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))

    fun normalDistFrom(start: Point): Array<IntArray> {
        val dist = Array(h) { IntArray(w) { -1 } }
        dist[start.x][start.y] = 0
        val q = LinkedList<Point>()
        q.add(start)
        while (q.isNotEmpty()) {
            val cur = q.poll()
            for (d in dirs) {
                val nx = cur.x + d.x
                val ny = cur.y + d.y
                if (nx in 0 until h && ny in 0 until w && !walls[nx][ny] && dist[nx][ny] == -1) {
                    dist[nx][ny] = dist[cur.x][cur.y] + 1
                    q.add(Point(nx, ny))
                }
            }
        }
        return dist
    }

    val distFromS = normalDistFrom(start)
    val distFromE = normalDistFrom(end)

    if (distFromS[end.x][end.y] == -1) {
        println(0)
        return
    }

    val normalCost = distFromS[end.x][end.y]
    val cheats = mutableMapOf<Pair<Point, Point>, Int>()

    for (startPos in trackCells) {
        val sd = distFromS[startPos.x][startPos.y]
        if (sd == -1) continue

        val distC = Array(h) { IntArray(w) { -1 } }
        distC[startPos.x][startPos.y] = 0
        val q = LinkedList<Point>()
        q.add(startPos)

        while (q.isNotEmpty()) {
            val cur = q.poll()
            val steps = distC[cur.x][cur.y]
            if (steps == 20) continue
            for (d in dirs) {
                val nx = cur.x + d.x
                val ny = cur.y + d.y
                if (nx in 0 until h && ny in 0 until w && distC[nx][ny] == -1) {
                    distC[nx][ny] = steps + 1
                    q.add(Point(nx, ny))
                }
            }
        }

        for (x in 0 until h) {
            for (y in 0 until w) {
                val s = distC[x][y]
                if (s in 1..20 && !walls[x][y]) {
                    val ed = distFromE[x][y]
                    if (ed == -1) continue
                    val cost = sd + s + ed
                    if (cost < normalCost) {
                        val key = Pair(startPos, Point(x, y))
                        if (!cheats.containsKey(key) || cost < cheats[key]!!) {
                            cheats[key] = cost
                        }
                    }
                }
            }
        }
    }

    var count = 0
    for (cost in cheats.values) {
        if (normalCost - cost >= 100) {
            count++
        }
    }
    println(count)
}

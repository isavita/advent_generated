
import java.io.File
import java.util.*
import kotlin.math.min

fun main() {
    val grid = File("input.txt").readLines()
    val n = grid.size
    val m = grid[0].length
    var sx = 0
    var sy = 0
    var ex = 0
    var ey = 0

    for (i in 0 until n) {
        for (j in 0 until m) {
            when (grid[i][j]) {
                'S' -> {
                    sx = i
                    sy = j
                }
                'E' -> {
                    ex = i
                    ey = j
                }
            }
        }
    }

    val dx = intArrayOf(-1, 0, 1, 0)
    val dy = intArrayOf(0, 1, 0, -1)

    val dist = Array(n) { Array(m) { IntArray(4) { Int.MAX_VALUE } } }
    dist[sx][sy][1] = 0

    val pq = PriorityQueue<Node>(compareBy { it.cost })
    pq.offer(Node(sx, sy, 1, 0))

    while (pq.isNotEmpty()) {
        val u = pq.poll()
        if (dist[u.x][u.y][u.d] < u.cost) {
            continue
        }
        if (u.x == ex && u.y == ey) {
            continue
        }

        for (ndir in intArrayOf((u.d + 1) % 4, (u.d + 3) % 4)) {
            val nc = u.cost + 1000
            if (nc < dist[u.x][u.y][ndir]) {
                dist[u.x][u.y][ndir] = nc
                pq.offer(Node(u.x, u.y, ndir, nc))
            }
        }

        val nx = u.x + dx[u.d]
        val ny = u.y + dy[u.d]
        if (nx in 0 until n && ny in 0 until m && grid[nx][ny] != '#') {
            val nc = u.cost + 1
            if (nc < dist[nx][ny][u.d]) {
                dist[nx][ny][u.d] = nc
                pq.offer(Node(nx, ny, u.d, nc))
            }
        }
    }

    var best = Int.MAX_VALUE
    for (d in 0 until 4) {
        best = min(best, dist[ex][ey][d])
    }

    val used = Array(n) { BooleanArray(m) }

    val rev = mutableListOf<State>()
    for (d in 0 until 4) {
        if (dist[ex][ey][d] == best) {
            rev.add(State(ex, ey, d))
        }
    }

    val vis = Array(n) { Array(m) { BooleanArray(4) } }
    for (s in rev) {
        vis[s.x][s.y][s.d] = true
    }

    while (rev.isNotEmpty()) {
        val u = rev.removeLast()
        used[u.x][u.y] = true

        val costU = dist[u.x][u.y][u.d]

        for (pd in intArrayOf((u.d + 1) % 4, (u.d + 3) % 4)) {
            if (dist[u.x][u.y][pd] == costU - 1000) {
                if (!vis[u.x][u.y][pd]) {
                    vis[u.x][u.y][pd] = true
                    rev.add(State(u.x, u.y, pd))
                }
            }
        }

        val px = u.x - dx[u.d]
        val py = u.y - dy[u.d]
        if (px in 0 until n && py in 0 until m && grid[px][py] != '#') {
            if (dist[px][py][u.d] == costU - 1) {
                if (!vis[px][py][u.d]) {
                    vis[px][py][u.d] = true
                    rev.add(State(px, py, u.d))
                }
            }
        }
    }

    var cnt = 0
    for (i in 0 until n) {
        for (j in 0 until m) {
            if (used[i][j] && grid[i][j] != '#') {
                cnt++
            }
        }
    }

    println(cnt)
}

data class Node(val x: Int, val y: Int, val d: Int, val cost: Int)
data class State(val x: Int, val y: Int, val d: Int)

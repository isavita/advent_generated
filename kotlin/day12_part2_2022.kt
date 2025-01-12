
import java.io.File
import java.util.*

fun main() {
    val grid = mutableMapOf<Pair<Int, Int>, Char>()
    var start = Pair(0, 0)
    var end = Pair(0, 0)
    val asList = mutableListOf<Pair<Int, Int>>()
    var y = 0
    File("input.txt").forEachLine { line ->
        line.forEachIndexed { x, char ->
            val p = Pair(x, y)
            grid[p] = char
            when (char) {
                'S' -> start = p
                'E' -> end = p
                'a' -> asList.add(p)
            }
        }
        y++
    }
    grid[start] = 'a'
    grid[end] = 'z'

    val dists = dijkstra(grid, end)

    var minDistance = dists[start] ?: Int.MAX_VALUE

    for (a in asList) {
        val dist = dists[a]
        if (dist != null) {
            minDistance = minOf(minDistance, dist)
        }
    }
    println(minDistance)
}

val neighbors = listOf(Pair(0, 1), Pair(0, -1), Pair(1, 0), Pair(-1, 0))

fun dijkstra(grid: Map<Pair<Int, Int>, Char>, end: Pair<Int, Int>): Map<Pair<Int, Int>, Int> {
    val dist = mutableMapOf<Pair<Int, Int>, Int>()
    val pq = PriorityQueue<Pair<Pair<Int, Int>, Int>>(compareBy { it.second })
    pq.add(Pair(end, 0))
    dist[end] = 0

    while (pq.isNotEmpty()) {
        val (curr, d) = pq.poll()

        for (n in neighbors) {
            val next = Pair(curr.first + n.first, curr.second + n.second)
            val nextChar = grid[next] ?: continue
            val currChar = grid[curr] ?: continue

            if (currChar.code - nextChar.code > 1) continue

            val nextDist = d + 1
            if (dist[next] == null || nextDist < dist[next]!!) {
                dist[next] = nextDist
                pq.add(Pair(next, nextDist))
            }
        }
    }
    return dist
}

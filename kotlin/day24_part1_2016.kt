import java.io.File
import java.util.*

data class Point(val x: Int, val y: Int)

fun readInput(fileName: String): List<String> {
    return File(fileName).readLines()
}

fun findPointsOfInterest(grid: List<String>): Map<Int, Point> {
    val points = mutableMapOf<Int, Point>()
    for (y in grid.indices) {
        for (x in grid[y].indices) {
            if (grid[y][x].isDigit()) {
                points[grid[y][x] - '0'] = Point(x, y)
            }
        }
    }
    return points
}

fun bfs(grid: List<String>, start: Point, end: Point): Int {
    val directions = listOf(Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0))
    val queue = LinkedList<Pair<Point, Int>>()
    val visited = mutableSetOf<Point>()

    queue.add(start to 0)
    visited.add(start)

    while (queue.isNotEmpty()) {
        val (current, distance) = queue.poll()

        if (current == end) return distance

        for (dir in directions) {
            val next = Point(current.x + dir.x, current.y + dir.y)
            if (next.x in grid[0].indices && next.y in grid.indices &&
                grid[next.y][next.x] != '#' && next !in visited) {
                queue.add(next to distance + 1)
                visited.add(next)
            }
        }
    }
    return Int.MAX_VALUE
}

fun shortestPathToVisitAllPoints(grid: List<String>, points: Map<Int, Point>): Int {
    val n = points.size
    val dist = Array(n) { IntArray(n) }

    val pointList = points.values.toList()
    for (i in 0 until n) {
        for (j in i + 1 until n) {
            val distance = bfs(grid, pointList[i], pointList[j])
            dist[i][j] = distance
            dist[j][i] = distance
        }
    }

    val allPoints = (1 until n).toList()
    var minDistance = Int.MAX_VALUE

    fun dfs(current: Int, visited: MutableSet<Int>, distance: Int) {
        if (visited.size == n - 1) {
            minDistance = minOf(minDistance, distance)
            return
        }
        for (next in allPoints) {
            if (next !in visited) {
                visited.add(next)
                dfs(next, visited, distance + dist[current][next])
                visited.remove(next)
            }
        }
    }

    dfs(0, mutableSetOf(), 0)
    return minDistance
}

fun main() {
    val grid = readInput("input.txt")
    val points = findPointsOfInterest(grid)
    val shortestPathLength = shortestPathToVisitAllPoints(grid, points)
    println(shortestPathLength)
}
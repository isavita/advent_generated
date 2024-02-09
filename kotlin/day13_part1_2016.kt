import java.io.File

data class Point(val x: Int, val y: Int)

fun isWall(favoriteNumber: Int, x: Int, y: Int): Boolean {
    var num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber
    var bits = 0
    while (num > 0) {
        if (num % 2 == 1) {
            bits++
        }
        num /= 2
    }
    return bits % 2 != 0
}

fun bfs(start: Point, target: Point, favoriteNumber: Int): Int {
    val visited = mutableSetOf<Point>()
    val queue = mutableListOf(start)
    var steps = 0

    while (queue.isNotEmpty()) {
        val size = queue.size
        for (i in 0 until size) {
            val point = queue[i]
            if (point == target) {
                return steps
            }

            val deltas = listOf(Point(1, 0), Point(-1, 0), Point(0, 1), Point(0, -1))
            for (delta in deltas) {
                val next = Point(point.x + delta.x, point.y + delta.y)
                if (next.x >= 0 && next.y >= 0 && !isWall(favoriteNumber, next.x, next.y) && !visited.contains(next)) {
                    visited.add(next)
                    queue.add(next)
                }
            }
        }
        queue.subList(0, size).clear()
        steps++
    }

    return -1
}

fun main(args: Array<String>) {
    val n = File("input.txt").readText().trim().toInt()
    val favoriteNumber = n
    val start = Point(1, 1)
    val target = Point(31, 39)
    val steps = bfs(start, target, favoriteNumber)
    println(steps)
}
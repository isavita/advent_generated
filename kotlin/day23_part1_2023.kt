import java.io.File
import java.util.Stack

data class Point(val x: Int, val y: Int)

fun main() {
    val input = File("input.txt").readLines()
    val map = input.map { it.toCharArray() }.toTypedArray()

    val start = Point(input[0].indexOf('.'), 0)
    val end = Point(input.last().indexOf('.'), input.size - 1)

    val longestPath = dfs(map, start, end)
    println("The longest hike is $longestPath steps long.")
}

fun dfs(map: Array<CharArray>, start: Point, end: Point): Int {
    val directions = mapOf(
        '>' to Point(1, 0),
        'v' to Point(0, 1),
        '<' to Point(-1, 0),
        '^' to Point(0, -1)
    )

    val maxPath = mutableListOf<Int>()
    val stack = Stack<Pair<Point, Int>>()
    stack.push(start to 0)

    while (stack.isNotEmpty()) {
        val (current, steps) = stack.pop()
        if (current == end) {
            maxPath.add(steps)
            continue
        }

        val nextPoints = mutableListOf<Point>()
        for ((dx, dy) in listOf(Point(1, 0), Point(0, 1), Point(-1, 0), Point(0, -1))) {
            val next = Point(current.x + dx, current.y + dy)
            if (next.x in map[0].indices && next.y in map.indices && map[next.y][next.x] != '#') {
                nextPoints.add(next)
            }
        }

        for (next in nextPoints) {
            val nextChar = map[next.y][next.x]
            if (nextChar == '.') {
                map[next.y][next.x] = '#' // Mark as visited
                stack.push(next to steps + 1)
            } else if (nextChar in directions) {
                val slopeDirection = directions[nextChar]!!
                val slopeNext = Point(next.x + slopeDirection.x, next.y + slopeDirection.y)
                if (slopeNext.x in map[0].indices && slopeNext.y in map.indices && map[slopeNext.y][slopeNext.x] != '#') {
                    map[next.y][next.x] = '#' // Mark as visited
                    stack.push(slopeNext to steps + 2)
                }
            }
        }
    }

    return maxPath.maxOrNull() ?: 0
}
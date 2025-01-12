
import java.io.File
import java.util.*
import kotlin.math.min

data class Point(val x: Int, val y: Int)
data class State(val point: Point, val level: Int, val steps: Int)

fun main() {
    val lines = File("input.txt").readLines()
    val maze = lines.map { it.toCharArray() }
    val height = maze.size
    val width = maze[0].size

    val portals = mutableMapOf<String, MutableList<Point>>()
    for (y in 0 until height) {
        for (x in 0 until width) {
            if (maze[y][x].isLetter()) {
                if (x + 1 < width && maze[y][x + 1].isLetter()) {
                    val label = "${maze[y][x]}${maze[y][x + 1]}"
                    val portalPoint = if (x + 2 < width && maze[y][x + 2] == '.') Point(x + 2, y) else Point(x - 1, y)
                    portals.computeIfAbsent(label) { mutableListOf() }.add(portalPoint)
                } else if (y + 1 < height && maze[y + 1][x].isLetter()) {
                    val label = "${maze[y][x]}${maze[y + 1][x]}"
                    val portalPoint = if (y + 2 < height && maze[y + 2][x] == '.') Point(x, y + 2) else Point(x, y - 1)
                    portals.computeIfAbsent(label) { mutableListOf() }.add(portalPoint)
                }
            }
        }
    }

    val start = portals["AA"]?.first() ?: throw Exception("Start not found")
    val end = portals["ZZ"]?.first() ?: throw Exception("End not found")

    fun solve(part2: Boolean): Int {
        val queue = LinkedList<State>()
        queue.add(State(start, 0, 0))
        val visited = mutableSetOf<Triple<Int, Int, Int>>()
        visited.add(Triple(start.x, start.y, 0))

        val directions = listOf(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0))

        while (queue.isNotEmpty()) {
            val current = queue.poll()
            if (current.point == end && current.level == 0) {
                return current.steps
            }

            for (dir in directions) {
                val nextPoint = Point(current.point.x + dir.x, current.point.y + dir.y)
                if (nextPoint.x in 0 until width && nextPoint.y in 0 until height && maze[nextPoint.y][nextPoint.x] == '.') {
                    val nextState = State(nextPoint, current.level, current.steps + 1)
                    if (visited.add(Triple(nextState.point.x, nextState.point.y, nextState.level))) {
                        queue.add(nextState)
                    }
                }
            }

            for ((label, points) in portals) {
                if (label == "AA" || label == "ZZ") continue
                if (current.point in points) {
                    val otherPoint = points.first { it != current.point }
                    val levelChange = if (isOuterPortal(current.point, maze)) -1 else 1
                    if (part2) {
                        val nextLevel = current.level + levelChange
                        if (nextLevel >= 0) {
                            val nextState = State(otherPoint, nextLevel, current.steps + 1)
                            if (visited.add(Triple(nextState.point.x, nextState.point.y, nextState.level))) {
                                queue.add(nextState)
                            }
                        }
                    } else {
                        val nextState = State(otherPoint, current.level, current.steps + 1)
                        if (visited.add(Triple(nextState.point.x, nextState.point.y, nextState.level))) {
                            queue.add(nextState)
                        }
                    }
                }
            }
        }
        return -1
    }

    println("Part 1: ${solve(false)}")
    println("Part 2: ${solve(true)}")
}

fun isOuterPortal(point: Point, maze: List<CharArray>): Boolean {
    val height = maze.size
    val width = maze[0].size
    return point.x == 2 || point.x == width - 3 || point.y == 2 || point.y == height - 3
}

import java.io.File
import java.util.LinkedList
import java.util.Queue

data class Point(val x: Int, val y: Int)

fun main() {
    val grid = readInput("input.txt")
    val start = findPosition(grid, 'S')
    val end = findPosition(grid, 'E')

    if (start == null || end == null) {
        println("Start or end position not found.")
        return
    }

    val shortestPath = findShortestPath(grid, start, end)
    println("The fewest steps required to move from 'S' to 'E' is $shortestPath.")
}

fun readInput(filename: String): List<String> {
    return File(filename).readLines()
}

fun findPosition(grid: List<String>, char: Char): Point? {
    for (y in grid.indices) {
        for (x in grid[y].indices) {
            if (grid[y][x] == char) {
                return Point(x, y)
            }
        }
    }
    return null
}

fun findShortestPath(grid: List<String>, start: Point, end: Point): Int {
    val directions = listOf(
        Point(0, 1), Point(1, 0), Point(0, -1), Point(-1, 0)
    )
    val queue: Queue<Pair<Point, Int>> = LinkedList()
    val visited = mutableSetOf<Point>()

    queue.add(Pair(start, 0))
    visited.add(start)

    while (queue.isNotEmpty()) {
        val (current, steps) = queue.poll()

        if (current == end) {
            return steps
        }

        for (direction in directions) {
            val next = Point(current.x + direction.x, current.y + direction.y)
            if (next.x in 0 until grid[0].length && next.y in 0 until grid.size && !visited.contains(next)) {
                val currentElevation = if (grid[current.y][current.x] == 'S') 'a' else grid[current.y][current.x]
                val nextElevation = if (grid[next.y][next.x] == 'E') 'z' else grid[next.y][next.x]

                if (nextElevation - currentElevation <= 1) {
                    queue.add(Pair(next, steps + 1))
                    visited.add(next)
                }
            }
        }
    }

    return -1
}
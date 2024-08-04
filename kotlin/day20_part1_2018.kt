import java.io.File

data class Point(val x: Int, val y: Int)
typealias DoorMap = Map<Point, Map<Point, Boolean>>

fun main() {
    val data = File("input.txt").readText()
    val regex = data.substring(1, data.length - 1)
    val dm = buildMap(regex)
    val maxDoors = findFurthestRoom(dm)
    println(maxDoors)
}

fun buildMap(regex: String): DoorMap {
    val dm = mutableMapOf<Point, MutableMap<Point, Boolean>>()
    val stack = mutableListOf<Point>()
    var cp = Point(0, 0)

    for (c in regex) {
        when (c) {
            '(' -> stack.add(cp)
            '|' -> cp = stack.last()
            ')' -> {
                cp = stack.last()
                stack.removeAt(stack.lastIndex)
            }
            else -> {
                val np = move(cp, c)
                dm.computeIfAbsent(cp) { mutableMapOf() }[np] = true
                cp = np
            }
        }
    }
    return dm
}

fun move(p: Point, dir: Char): Point {
    return when (dir) {
        'N' -> Point(p.x, p.y - 1)
        'S' -> Point(p.x, p.y + 1)
        'E' -> Point(p.x + 1, p.y)
        'W' -> Point(p.x - 1, p.y)
        else -> p
    }
}

fun findFurthestRoom(dm: DoorMap): Int {
    val visited = mutableMapOf<Point, Int>()
    val queue = mutableListOf(Point(0, 0))
    var maxDoors = 0

    visited[Point(0, 0)] = 0 // Initialize the starting point

    while (queue.isNotEmpty()) {
        val p = queue.removeAt(0)
        val connectedPoints = dm[p] ?: continue // Skip if no connections

        for (np in connectedPoints.keys) {
            if (np !in visited) {
                visited[np] = visited[p]!! + 1
                maxDoors = maxOf(maxDoors, visited[np]!!)
                queue.add(np)
            }
        }
    }
    return maxDoors
}
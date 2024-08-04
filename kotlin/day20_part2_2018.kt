import java.io.File

data class Point(val x: Int, val y: Int)

typealias DoorMap = MutableMap<Point, MutableMap<Point, Boolean>>

fun main() {
    val data = File("input.txt").readText()
    val regex = data.substring(1, data.length - 1)
    val dm = buildMap(regex)
    val rooms = countRooms(dm, 1000)
    println(rooms)
}

fun buildMap(regex: String): DoorMap {
    val dm = mutableMapOf<Point, MutableMap<Point, Boolean>>()
    val stack = mutableListOf<Point>()
    var cp = Point(0, 0)
    dm[cp] = mutableMapOf()
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
                if (!dm.containsKey(np)) {
                    dm[np] = mutableMapOf()
                }
                dm[cp]!![np] = true
                dm[np]!![cp] = true // Add door in the other direction
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

fun countRooms(dm: DoorMap, minDoors: Int): Int {
    val visited = mutableMapOf<Point, Int>()
    val queue = mutableListOf(Point(0, 0))
    var roomCount = 0

    while (queue.isNotEmpty()) {
        val p = queue.removeAt(0)
        for (np in dm[p]!!.keys) {
            if (!visited.containsKey(np)) {
                visited[np] = visited[p]?.plus(1) ?: 1
                if (visited[np]!! >= minDoors) {
                    roomCount++
                }
                queue.add(np)
            }
        }
    }
    return roomCount
}
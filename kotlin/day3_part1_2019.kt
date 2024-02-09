import java.io.File

data class Point(val X: Int, val Y: Int)

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val wire1 = getPoints(lines[0])
    val wire2 = getPoints(lines[1])

    val intersections = mutableMapOf<Point, Boolean>()
    for (p in wire1.keys) {
        if (wire2.containsKey(p)) {
            intersections[p] = true
        }
    }

    var minDistance = Int.MAX_VALUE
    for (p in intersections.keys) {
        val distance = Math.abs(p.X) + Math.abs(p.Y)
        if (distance < minDistance) {
            minDistance = distance
        }
    }

    println(minDistance)
}

fun getPoints(path: String): Map<Point, Boolean> {
    val points = mutableMapOf<Point, Boolean>()
    var current = Point(0, 0)
    path.split(",").forEach { move ->
        val dir = move[0]
        val steps = move.substring(1).toInt()
        repeat(steps) {
            when (dir) {
                'U' -> current = Point(current.X, current.Y + 1)
                'D' -> current = Point(current.X, current.Y - 1)
                'L' -> current = Point(current.X - 1, current.Y)
                'R' -> current = Point(current.X + 1, current.Y)
            }
            points[current] = true
        }
    }
    return points
}
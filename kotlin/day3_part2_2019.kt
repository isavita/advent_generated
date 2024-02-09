import java.io.File

data class Point(val X: Int, val Y: Int)

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val wire1 = getPointsWithSteps(lines[0])
    val wire2 = getPointsWithSteps(lines[1])

    var minSteps = Int.MAX_VALUE
    wire1.forEach { (p, steps1) ->
        wire2[p]?.let { steps2 ->
            val totalSteps = steps1 + steps2
            if (totalSteps < minSteps) {
                minSteps = totalSteps
            }
        }
    }

    println(minSteps)
}

fun getPointsWithSteps(path: String): Map<Point, Int> {
    val points = mutableMapOf<Point, Int>()
    var current = Point(0, 0)
    var steps = 0
    path.split(",").forEach { move ->
        val dir = move[0]
        val dist = move.substring(1).toInt()
        repeat(dist) {
            steps++
            when (dir) {
                'U' -> current = Point(current.X, current.Y + 1)
                'D' -> current = Point(current.X, current.Y - 1)
                'L' -> current = Point(current.X - 1, current.Y)
                'R' -> current = Point(current.X + 1, current.Y)
            }
            if (!points.containsKey(current)) {
                points[current] = steps
            }
        }
    }
    return points
}
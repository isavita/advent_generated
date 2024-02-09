import java.io.File

data class Point(val x: Int, val y: Int)

fun main(args: Array<String>) {
    val grid = mutableMapOf<Point, Int>()

    File("input.txt").forEachLine { line ->
        val coords = line.split(" -> ")
        val startCoords = coords[0].split(",")
        val endCoords = coords[1].split(",")

        val x1 = startCoords[0].toInt()
        val y1 = startCoords[1].toInt()
        val x2 = endCoords[0].toInt()
        val y2 = endCoords[1].toInt()

        if (x1 == x2) {
            if (y1 > y2) {
                for (y in y2..y1) {
                    grid.merge(Point(x1, y), 1, Int::plus)
                }
            } else {
                for (y in y1..y2) {
                    grid.merge(Point(x1, y), 1, Int::plus)
                }
            }
        } else if (y1 == y2) {
            if (x1 > x2) {
                for (x in x2..x1) {
                    grid.merge(Point(x, y1), 1, Int::plus)
                }
            } else {
                for (x in x1..x2) {
                    grid.merge(Point(x, y1), 1, Int::plus)
                }
            }
        }
    }

    val overlapCount = grid.count { it.value > 1 }

    println(overlapCount)
}
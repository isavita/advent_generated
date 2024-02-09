import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val lines = input.map { it.split(" -> ") }

    val points = mutableSetOf<Pair<Int, Int>>()
    val overlaps = mutableSetOf<Pair<Int, Int>>()

    for (line in lines) {
        val (start, end) = line.map { it.split(",") }
        val (x1, y1) = start.map { it.toInt() }
        val (x2, y2) = end.map { it.toInt() }

        if (x1 == x2) {
            for (y in minOf(y1, y2)..maxOf(y1, y2)) {
                val point = Pair(x1, y)
                if (point in points) {
                    overlaps.add(point)
                } else {
                    points.add(point)
                }
            }
        } else if (y1 == y2) {
            for (x in minOf(x1, x2)..maxOf(x1, x2)) {
                val point = Pair(x, y1)
                if (point in points) {
                    overlaps.add(point)
                } else {
                    points.add(point)
                }
            }
        } else {
            val dx = if (x1 < x2) 1 else -1
            val dy = if (y1 < y2) 1 else -1
            var x = x1
            var y = y1
            while (x != x2 && y != y2) {
                val point = Pair(x, y)
                if (point in points) {
                    overlaps.add(point)
                } else {
                    points.add(point)
                }
                x += dx
                y += dy
            }
            val point = Pair(x2, y2)
            if (point in points) {
                overlaps.add(point)
            } else {
                points.add(point)
            }
        }
    }

    println(overlaps.size)
}
import java.io.File

data class Point(var X: Int, var Y: Int)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val points = mutableMapOf<Point, Boolean>()
    val folds = mutableListOf<String>()
    var readingPoints = true

    file.forEachLine {
        if (it.isEmpty()) {
            readingPoints = false
            return@forEachLine
        }

        if (readingPoints) {
            val coords = it.split(",")
            val x = coords[0].toInt()
            val y = coords[1].toInt()
            points[Point(x, y)] = true
        } else {
            folds.add(it)
        }
    }

    val fold = folds[0].split(" ")[2]
    val axisValue = fold.split("=")
    val axis = axisValue[0]
    val value = axisValue[1].toInt()

    val newPoints = mutableMapOf<Point, Boolean>()
    if (axis == "x") {
        points.keys.forEach { point ->
            if (point.X > value) {
                point.X = 2 * value - point.X
            }
            newPoints[point] = true
        }
    } else {
        points.keys.forEach { point ->
            if (point.Y > value) {
                point.Y = 2 * value - point.Y
            }
            newPoints[point] = true
        }
    }

    println(newPoints.size)
}
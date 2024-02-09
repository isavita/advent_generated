import java.io.File

data class Point(val x: Int, val y: Int)

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val points = mutableListOf<Point>()
    var maxX = 0
    var maxY = 0

    lines.forEach {
        val coords = it.split(", ")
        val x = coords[0].toInt()
        val y = coords[1].toInt()
        if (x > maxX) {
            maxX = x
        }
        if (y > maxY) {
            maxY = y
        }
        points.add(Point(x, y))
    }

    val grid = Array(maxX + 2) { IntArray(maxY + 2) }
    val areas = IntArray(points.size)
    val infinite = BooleanArray(points.size)

    for (i in grid.indices) {
        for (j in grid[i].indices) {
            var minDist = maxX + maxY
            for (k in points.indices) {
                val dist = abs(points[k].x - i) + abs(points[k].y - j)
                if (dist < minDist) {
                    minDist = dist
                    grid[i][j] = k
                } else if (dist == minDist) {
                    grid[i][j] = -1
                }
            }
            if (grid[i][j] != -1) {
                if (i == 0 || j == 0 || i == maxX + 1 || j == maxY + 1) {
                    infinite[grid[i][j]] = true
                }
                areas[grid[i][j]]++
            }
        }
    }

    var maxArea = 0
    areas.forEachIndexed { index, area ->
        if (!infinite[index] && area > maxArea) {
            maxArea = area
        }
    }
    println(maxArea)
}

fun abs(x: Int) = if (x < 0) -x else x
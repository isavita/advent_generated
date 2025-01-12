
import java.io.File
import kotlin.math.abs
import kotlin.math.min
import kotlin.system.measureTimeMillis

fun main() {
    val lines = File("input.txt").readLines()
    val stars = lines.mapNotNull { line ->
        val match = Regex("""position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""").find(line)
        match?.destructured?.let { (x, y, vx, vy) ->
            Star(x.toInt(), y.toInt(), vx.toInt(), vy.toInt())
        }
    }

    var smallestT = 0
    var smallestArea = Int.MAX_VALUE

    for (t in 1..20000) {
        var maxX = Int.MIN_VALUE
        var maxY = Int.MIN_VALUE
        var minX = Int.MAX_VALUE
        var minY = Int.MAX_VALUE

        for (star in stars) {
            val x = star.x + star.vX * t
            val y = star.y + star.vY * t
            maxX = maxOf(maxX, x)
            minX = minOf(minX, x)
            maxY = maxOf(maxY, y)
            minY = minOf(minY, y)
        }

        val area = (maxX - minX + 1) + (maxY - minY + 1)

        if (smallestArea > area) {
            smallestArea = area
            smallestT = t
        }
    }

    val t = smallestT
    var maxX = Int.MIN_VALUE
    var maxY = Int.MIN_VALUE
    var minX = Int.MAX_VALUE
    var minY = Int.MAX_VALUE

    val points = stars.map { star ->
        val x = star.x + star.vX * t
        val y = star.y + star.vY * t
        maxX = maxOf(maxX, x)
        minX = minOf(minX, x)
        maxY = maxOf(maxY, y)
        minY = minOf(minY, y)
        x to y
    }

    val width = maxX - minX + 1
    val height = maxY - minY + 1
    val grid = Array(height) { BooleanArray(width) }

    for ((x, y) in points) {
        grid[y - minY][x - minX] = true
    }

    grid.forEach { row ->
        row.forEach { print(if (it) "#" else " ") }
        println()
    }
}

data class Star(val x: Int, val y: Int, val vX: Int, val vY: Int)

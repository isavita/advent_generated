
import java.io.File
import kotlin.math.abs

data class Coord(var x: Double, var y: Double, var z: Double)
data class Point(var pos: Coord, var vel: Coord)

fun parseInput(input: List<String>): List<Point> {
    return input.map { line ->
        val parts = line.split(" @ ", ", ").map { it.trim().toDouble() }
        Point(Coord(parts[0], parts[1], parts[2]), Coord(parts[3], parts[4], parts[5]))
    }
}

fun isIntersecting2D(p1: Point, p2: Point): Pair<Boolean, Coord?> {
    val det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
    if (abs(det) < 1e-9) return false to null

    val t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
    val t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det

    if (t1 < 0 || t2 < 0) return false to null

    val x = p1.pos.x + p1.vel.x * t1
    val y = p1.pos.y + p1.vel.y * t1
    return true to Coord(x, y, 0.0)
}

fun solve(input: List<String>, min: Double, max: Double): Int {
    val points = parseInput(input)
    var count = 0
    for (i in points.indices) {
        for (j in 0 until i) {
            val (isIntersecting, coord) = isIntersecting2D(points[i], points[j])
            if (isIntersecting) {
                coord?.let {
                    if (it.x >= min && it.x <= max && it.y >= min && it.y <= max) {
                        count++
                    }
                }
            }
        }
    }
    return count
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input, 200000000000000.0, 400000000000000.0))
}

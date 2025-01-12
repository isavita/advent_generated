
import java.io.File
import kotlin.math.abs

data class Coord(val x: Long, val y: Long) {
    operator fun plus(other: Coord) = Coord(x + other.x, y + other.y)
    operator fun times(scalar: Long) = Coord(x * scalar, y * scalar)
}

val North = Coord(0, -1)
val West = Coord(-1, 0)
val South = Coord(0, 1)
val East = Coord(1, 0)

fun parseInput(input: List<String>): List<Coord> {
    var current = Coord(0, 0)
    val vertices = mutableListOf(current)

    for (line in input) {
        val parts = line.split(" ")
        val color = parts[2]
        val dirInput = color[7]
        val lengthStr = color.substring(2, 7)
        val length = lengthStr.toLong(16)

        val dir = when (dirInput) {
            '3' -> North
            '2' -> West
            '1' -> South
            '0' -> East
            else -> throw IllegalArgumentException("Invalid direction: $dirInput")
        }

        current += dir * length
        vertices.add(current)
    }
    return vertices
}

fun shoelace(vertices: List<Coord>): Long {
    val n = vertices.size
    var area = 0L
    for (i in 0 until n) {
        val next = (i + 1) % n
        area += vertices[i].x * vertices[next].y
        area -= vertices[i].y * vertices[next].x
    }
    return abs(area) / 2
}

fun perimeter(vertices: List<Coord>): Long {
    val n = vertices.size
    var perim = 0L
    for (i in 0 until n) {
        val next = (i + 1) % n
        perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)
    }
    return perim
}

fun calculatePolygonArea(vertices: List<Coord>): Long {
    return shoelace(vertices) + perimeter(vertices) / 2 + 1
}

fun solve(input: List<String>): Long {
    val vertices = parseInput(input)
    return calculatePolygonArea(vertices)
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}

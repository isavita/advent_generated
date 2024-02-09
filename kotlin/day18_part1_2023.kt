import java.io.File

data class Coord(val x: Int, val y: Int) {
    fun add(c: Coord) = Coord(x + c.x, y + c.y)
    fun multiplyByScalar(s: Int) = Coord(x * s, y * s)
}

val north = Coord(0, -1)
val west = Coord(-1, 0)
val south = Coord(0, 1)
val east = Coord(1, 0)

fun abs(x: Int) = if (x < 0) -x else x

fun parseInput(input: List<String>): List<Coord> {
    var current = Coord(0, 0)
    val vertices = mutableListOf(current)

    for (line in input) {
        val parts = line.split(" ")
        val dirInput = parts[0][0]
        val lengthStr = parts[1]
        var length = 0
        for (c in lengthStr) {
            length = length * 10 + (c - '0')
        }

        val dir = when (dirInput) {
            'U' -> north
            'L' -> west
            'D' -> south
            'R' -> east
            else -> throw IllegalArgumentException("Invalid direction")
        }

        current = current.add(dir.multiplyByScalar(length))
        vertices.add(current)
    }

    return vertices
}

fun hexStringToInt(hexStr: String) = hexStr.toInt(16)

fun shoelace(vertices: List<Coord>): Int {
    val n = vertices.size
    var area = 0

    for (i in 0 until n) {
        val next = (i + 1) % n
        area += vertices[i].x * vertices[next].y
        area -= vertices[i].y * vertices[next].x
    }

    area = abs(area) / 2
    return area
}

fun perimeter(vertices: List<Coord>): Int {
    val n = vertices.size
    var perim = 0

    for (i in 0 until n) {
        val next = (i + 1) % n
        perim += abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)
    }

    return perim
}

fun calculatePolygonArea(vertices: List<Coord>) = shoelace(vertices) + perimeter(vertices) / 2 + 1

fun solve(input: List<String>): Int {
    val vertices = parseInput(input)
    return calculatePolygonArea(vertices)
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}
import java.io.File
import kotlin.math.abs

fun main(args: Array<String>) {
    val content = File("input.txt").readText()
    val coordinates = parseCoordinates(content)
    val regionSize = findRegionSize(coordinates, 10000)
    println(regionSize)
}

data class Coordinate(val x: Int, val y: Int)

fun parseCoordinates(input: String): List<Coordinate> {
    val lines = input.trim().split("\n")
    return lines.map {
        val (x, y) = it.split(", ").map { it.toInt() }
        Coordinate(x, y)
    }
}

fun findRegionSize(coordinates: List<Coordinate>, maxDistance: Int): Int {
    val (minX, minY, maxX, maxY) = findBoundingBox(coordinates)
    var regionSize = 0

    for (x in minX..maxX) {
        for (y in minY..maxY) {
            var totalDistance = 0

            for (c in coordinates) {
                totalDistance += manhattanDistance(x, y, c.x, c.y)
            }

            if (totalDistance < maxDistance) {
                regionSize++
            }
        }
    }

    return regionSize
}

fun findBoundingBox(coordinates: List<Coordinate>): List<Int> {
    var minX = Int.MAX_VALUE
    var minY = Int.MAX_VALUE
    var maxX = Int.MIN_VALUE
    var maxY = Int.MIN_VALUE

    for (c in coordinates) {
        minX = minOf(minX, c.x)
        minY = minOf(minY, c.y)
        maxX = maxOf(maxX, c.x)
        maxY = maxOf(maxY, c.y)
    }

    return listOf(minX, minY, maxX, maxY)
}

fun manhattanDistance(x1: Int, y1: Int, x2: Int, y2: Int): Int {
    return abs(x1 - x2) + abs(y1 - y2)
}
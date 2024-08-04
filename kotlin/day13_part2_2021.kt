import java.io.File

data class Coordinate(val x: Int, val y: Int)

fun main() {
    val input = File("input.txt").readLines()
    val coordinates = input
        .takeWhile { it.contains(',') }
        .map { it.split(',') }
        .map { Coordinate(it[0].toInt(), it[1].toInt()) }
        .toSet()

    val folds = input
        .dropWhile { !it.startsWith("fold") }
        .map {
            val (axis, value) = it.split('=')
            axis to value.toInt()
        }

    val part1 = part1(coordinates, folds)
    println("Part 1: $part1")

    val part2 = part2(coordinates, folds)
    println("Part 2: $part2")
}

fun part1(coordinates: Set<Coordinate>, folds: List<Pair<String, Int>>): Int {
    val (axis, value) = folds[0]
    return fold(coordinates, axis, value).size
}

fun part2(coordinates: Set<Coordinate>, folds: List<Pair<String, Int>>): String {
    var current = coordinates
    for ((axis, value) in folds) {
        current = fold(current, axis, value)
    }
    return printCoordinates(current)
}

fun fold(coordinates: Set<Coordinate>, axis: String, value: Int): Set<Coordinate> {
    return when (axis) {
        "fold along x" -> coordinates.map { if (it.x > value) Coordinate(2 * value - it.x, it.y) else it }.toSet()
        "fold along y" -> coordinates.map { if (it.y > value) Coordinate(it.x, 2 * value - it.y) else it }.toSet()
        else -> throw Exception("Invalid axis")
    }
}

fun printCoordinates(coordinates: Set<Coordinate>): String {
    val maxX = coordinates.maxOf { it.x }
    val maxY = coordinates.maxOf { it.y }
    val grid = Array(maxY + 1) { CharArray(maxX + 1) { '.' } }
    for ((x, y) in coordinates) {
        grid[y][x] = '#'
    }
    return grid.joinToString("\n") { it.joinToString("") }
}
import java.io.File

data class Point(val x: Int, val y: Int)

fun main() {
    val input = File("input.txt").readLines()
    val rockStructures = parseRockStructures(input)
    val grid = simulateSand(rockStructures)
    println(grid.values.count { it == 'o' })
}

fun parseRockStructures(input: List<String>): List<List<Point>> {
    val rockStructures = mutableListOf<List<Point>>()

    for (line in input) {
        val points = line.split(" -> ").map {
            val (x, y) = it.split(",").map { it.toInt() }
            Point(x, y)
        }
        rockStructures.add(points)
    }

    return rockStructures
}

fun simulateSand(rockStructures: List<List<Point>>): MutableMap<Point, Char> {
    val grid = mutableMapOf<Point, Char>()
    val sandSource = Point(500, 0)

    // Draw the rock structures
    for (structure in rockStructures) {
        for (i in 0 until structure.size - 1) {
            val (x1, y1) = structure[i]
            val (x2, y2) = structure[i + 1]
            if (x1 == x2) {
                for (y in minOf(y1, y2)..maxOf(y1, y2)) {
                    grid[Point(x1, y)] = '#'
                }
            } else {
                for (x in minOf(x1, x2)..maxOf(x1, x2)) {
                    grid[Point(x, y1)] = '#'
                }
            }
        }
    }

    // Simulate the falling sand
    var sandCount = 0
    while (true) {
        var sand = sandSource
        while (true) {
            if (!grid.containsKey(Point(sand.x, sand.y + 1))) {
                sand = Point(sand.x, sand.y + 1)
            } else if (!grid.containsKey(Point(sand.x - 1, sand.y + 1))) {
                sand = Point(sand.x - 1, sand.y + 1)
            } else if (!grid.containsKey(Point(sand.x + 1, sand.y + 1))) {
                sand = Point(sand.x + 1, sand.y + 1)
            } else {
                grid[sand] = 'o'
                sandCount++
                break
            }

            if (sand.y >= grid.keys.maxOf { it.y }) {
                return grid
            }
        }
    }
}
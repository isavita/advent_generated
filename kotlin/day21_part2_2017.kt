
import java.io.File

fun main() {
    val rules = File("input.txt").readLines().associate {
        val (key, value) = it.split(" => ")
        key to value
    }

    var grid = arrayOf(
        ".#.".toCharArray(),
        "..#".toCharArray(),
        "###".toCharArray()
    )

    repeat(18) {
        val subSize = if (grid.size % 2 == 0) 2 else 3
        val newSize = grid.size / subSize * (subSize + 1)
        val newGrid = Array(newSize) { CharArray(newSize) }

        for (y in grid.indices step subSize) {
            for (x in grid.indices step subSize) {
                val square = (y until y + subSize).map { row ->
                    grid[row].slice(x until x + subSize).joinToString("")
                }.joinToString("/")

                val newSquare = enhance(square, rules)
                newSquare.split("/").forEachIndexed { dy, row ->
                    row.forEachIndexed { dx, char ->
                        newGrid[y / subSize * (subSize + 1) + dy][x / subSize * (subSize + 1) + dx] = char
                    }
                }
            }
        }
        grid = newGrid
    }

    val count = grid.sumOf { row -> row.count { it == '#' } }
    println(count)
}

private val memo = mutableMapOf<String, String>()

fun enhance(input: String, rules: Map<String, String>): String {
    memo[input]?.let { return it }

    var current = input
    repeat(4) {
        rules[current]?.let {
            memo[input] = it
            return it
        }
        current = rotate(current)
    }
    current = flip(input)
    repeat(4) {
        rules[current]?.let {
            memo[input] = it
            return it
        }
        current = rotate(current)
    }
    return ""
}

fun rotate(input: String): String {
    val parts = input.split("/")
    val size = parts.size
    return (0 until size).joinToString("/") { x ->
        (size - 1 downTo 0).map { y -> parts[y][x] }.joinToString("")
    }
}

fun flip(input: String): String {
    return input.split("/").joinToString("/") { it.reversed() }
}

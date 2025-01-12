
import java.io.File
import kotlin.math.max

fun main() {
    val grid = File("input.txt").readLines().map { it.toCharArray() }
    val start = findStart(grid)
    val loop = findLoop(grid, start)
    println(loop.size / 2)
}

fun findStart(grid: List<CharArray>): Pair<Int, Int> {
    for (r in grid.indices) {
        for (c in grid[r].indices) {
            if (grid[r][c] == 'S') {
                return r to c
            }
        }
    }
    throw IllegalArgumentException("No start found")
}

fun findLoop(grid: List<CharArray>, start: Pair<Int, Int>): List<Pair<Int, Int>> {
    val rows = grid.size
    val cols = grid[0].size
    val visited = mutableSetOf<Pair<Int, Int>>()
    val loop = mutableListOf<Pair<Int, Int>>()
    var current = start
    var prev: Pair<Int, Int>? = null

    do {
        loop.add(current)
        visited.add(current)
        val (r, c) = current
        val next = when (grid[r][c]) {
            'S' -> findStartNeighbors(grid, start).firstOrNull { it != prev }
            '|' -> listOf(r - 1 to c, r + 1 to c).firstOrNull { it != prev }
            '-' -> listOf(r to c - 1, r to c + 1).firstOrNull { it != prev }
            'L' -> listOf(r - 1 to c, r to c + 1).firstOrNull { it != prev }
            'J' -> listOf(r - 1 to c, r to c - 1).firstOrNull { it != prev }
            '7' -> listOf(r + 1 to c, r to c - 1).firstOrNull { it != prev }
            'F' -> listOf(r + 1 to c, r to c + 1).firstOrNull { it != prev }
            else -> null
        }
        if (next == null) break
        prev = current
        current = next
    } while (current != start)
    return loop
}

fun findStartNeighbors(grid: List<CharArray>, start: Pair<Int, Int>): List<Pair<Int, Int>> {
    val (r, c) = start
    val neighbors = mutableListOf<Pair<Int, Int>>()
    val rows = grid.size
    val cols = grid[0].size

    // Check north
    if (r > 0 && "|7F".contains(grid[r - 1][c])) neighbors.add(r - 1 to c)
    // Check south
    if (r < rows - 1 && "|LJ".contains(grid[r + 1][c])) neighbors.add(r + 1 to c)
    // Check west
    if (c > 0 && "-LF".contains(grid[r][c - 1])) neighbors.add(r to c - 1)
    // Check east
    if (c < cols - 1 && "-J7".contains(grid[r][c + 1])) neighbors.add(r to c + 1)

    return neighbors
}


import java.io.File

fun main() {
    val grid = File("input.txt").readLines().map { it.toCharArray() }.toMutableList()
    println(findSafeStep(grid))
}

fun findSafeStep(grid: MutableList<CharArray>): Int {
    var step = 0
    while (true) {
        val eastMoved = moveEast(grid)
        val southMoved = moveSouth(grid)
        step++
        if (!eastMoved && !southMoved) break
    }
    return step
}

fun moveEast(grid: MutableList<CharArray>): Boolean {
    val height = grid.size
    val width = grid[0].size
    var moved = false
    val toMove = mutableListOf<Pair<Int, Int>>()

    for (y in 0 until height) {
        for (x in 0 until width) {
            if (grid[y][x] == '>') {
                val nextX = (x + 1) % width
                if (grid[y][nextX] == '.') {
                    toMove.add(Pair(y, x))
                    moved = true
                }
            }
        }
    }
    for ((y, x) in toMove) {
        val nextX = (x + 1) % width
        grid[y][x] = '.'
        grid[y][nextX] = '>'
    }
    return moved
}

fun moveSouth(grid: MutableList<CharArray>): Boolean {
    val height = grid.size
    val width = grid[0].size
    var moved = false
    val toMove = mutableListOf<Pair<Int, Int>>()

    for (x in 0 until width) {
        for (y in 0 until height) {
            if (grid[y][x] == 'v') {
                val nextY = (y + 1) % height
                if (grid[nextY][x] == '.') {
                    toMove.add(Pair(y, x))
                    moved = true
                }
            }
        }
    }
    for ((y, x) in toMove) {
        val nextY = (y + 1) % height
        grid[y][x] = '.'
        grid[nextY][x] = 'v'
    }
    return moved
}

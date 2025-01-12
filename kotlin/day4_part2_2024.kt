
import java.io.File

fun main() {
    val grid = File("input.txt").readLines().filter { it.isNotBlank() }
    val count = countXMASPatterns(grid)
    println("X-MAS patterns appear $count times in the word search")
}

fun checkMAS(grid: List<String>, x: Int, y: Int, dx: Int, dy: Int): Boolean {
    val word = "MAS"
    var forward = true
    var backward = true

    for (i in word.indices) {
        val newX = x + dx * i
        val newY = y + dy * i
        if (newX < 0 || newY < 0 || newX >= grid.size || newY >= grid[0].length) {
            forward = false
            break
        }
        if (grid[newX][newY] != word[i]) {
            forward = false
        }
    }

    for (i in word.indices) {
        val newX = x + dx * i
        val newY = y + dy * i
        if (newX < 0 || newY < 0 || newX >= grid.size || newY >= grid[0].length) {
            backward = false
            break
        }
        if (grid[newX][newY] != word[word.length - 1 - i]) {
            backward = false
        }
    }

    return forward || backward
}

fun checkXMAS(grid: List<String>, x: Int, y: Int): Boolean {
    return (checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1)) ||
            (checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1))
}

fun countXMASPatterns(grid: List<String>): Int {
    if (grid.size < 3 || grid[0].length < 3) return 0

    var count = 0
    for (i in 1 until grid.size - 1) {
        for (j in 1 until grid[i].length - 1) {
            if (grid[i][j] == 'A' && checkXMAS(grid, i, j)) {
                count++
            }
        }
    }
    return count
}

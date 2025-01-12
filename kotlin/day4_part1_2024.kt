
import java.io.File

fun main() {
    val grid = File("input.txt").readLines().filter { it.isNotBlank() }
    val count = countOccurrences(grid, "XMAS")
    println("XMAS appears $count times in the word search")
}

private fun countOccurrences(grid: List<String>, word: String): Int {
    if (grid.isEmpty() || word.isEmpty()) return 0
    val rows = grid.size
    val cols = grid[0].length
    var count = 0
    val directions = arrayOf(
        0 to 1, 1 to 0, 1 to 1, -1 to 1,
        0 to -1, -1 to 0, -1 to -1, 1 to -1
    )

    for (row in 0 until rows) {
        for (col in 0 until cols) {
            for ((dx, dy) in directions) {
                if (checkWord(grid, word, row, col, dx, dy)) {
                    count++
                }
            }
        }
    }
    return count
}

private fun checkWord(grid: List<String>, word: String, x: Int, y: Int, dx: Int, dy: Int): Boolean {
    val rows = grid.size
    val cols = grid[0].length
    for (i in word.indices) {
        val newX = x + dx * i
        val newY = y + dy * i
        if (newX < 0 || newY < 0 || newX >= rows || newY >= cols || grid[newX][newY] != word[i]) {
            return false
        }
    }
    return true
}

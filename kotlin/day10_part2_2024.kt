
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    val nr = lines.size
    val nc = lines[0].length
    val grid = Array(nr) { r -> IntArray(nc) { c -> lines[r][c] - '0' } }
    val dp = Array(nr) { LongArray(nc) { -1 } }
    val dirs = arrayOf(1 to 0, -1 to 0, 0 to 1, 0 to -1)

    fun dfs(r: Int, c: Int): Long {
        if (dp[r][c] != -1L) return dp[r][c]
        val h = grid[r][c]
        if (h == 9) {
            dp[r][c] = 1
            return 1
        }
        var sum = 0L
        for ((dr, dc) in dirs) {
            val nr2 = r + dr
            val nc2 = c + dc
            if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue
            if (grid[nr2][nc2] == h + 1) {
                sum += dfs(nr2, nc2)
            }
        }
        dp[r][c] = sum
        return sum
    }

    var total = 0L
    for (r in 0 until nr) {
        for (c in 0 until nc) {
            if (grid[r][c] == 0) {
                total += dfs(r, c)
            }
        }
    }
    println(total)
}

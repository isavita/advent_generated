
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    val availablePatterns = lines[0].split(",").map { it.trim() }
    var totalWays = 0L
    for (i in 2 until lines.size) {
        totalWays += countWays(lines[i].trim(), availablePatterns)
    }
    println(totalWays)
}

fun countWays(design: String, patterns: List<String>): Long {
    val n = design.length
    val dp = LongArray(n + 1)
    dp[0] = 1L
    for (i in 1..n) {
        for (p in patterns) {
            val lp = p.length
            if (i >= lp && design.substring(i - lp, i) == p) {
                dp[i] += dp[i - lp]
            }
        }
    }
    return dp[n]
}

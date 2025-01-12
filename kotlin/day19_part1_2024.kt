
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    val availablePatterns = lines[0].split(",").map { it.trim() }
    val designs = lines.drop(2)

    val count = designs.count { canMake(it, availablePatterns) }
    println(count)
}

fun canMake(design: String, patterns: List<String>): Boolean {
    val n = design.length
    val dp = BooleanArray(n + 1)
    dp[0] = true
    for (i in 1..n) {
        for (p in patterns) {
            val lp = p.length
            if (i >= lp && dp[i - lp] && design.substring(i - lp, i) == p) {
                dp[i] = true
                break
            }
        }
    }
    return dp[n]
}

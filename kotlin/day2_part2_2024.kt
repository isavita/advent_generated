
import java.io.File
import kotlin.math.abs

fun main() {
    val safeReportCount = File("input.txt").readLines().count { line ->
        val levels = line.split(" ").map { it.toInt() }
        isSafeReport(levels) || isSafeWithOneRemoval(levels)
    }
    println(safeReportCount)
}

fun isSafeReport(levels: List<Int>): Boolean {
    if (levels.size < 2) return false

    val firstDiff = levels[1] - levels[0]
    if (firstDiff == 0) return false

    val isIncreasing = firstDiff > 0

    for (i in 0 until levels.size - 1) {
        val diff = levels[i + 1] - levels[i]
        if (diff == 0) return false
        if ((isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0)) return false
        val absDiff = abs(diff)
        if (absDiff < 1 || absDiff > 3) return false
    }
    return true
}

fun isSafeWithOneRemoval(levels: List<Int>): Boolean {
    for (i in levels.indices) {
        val modifiedLevels = levels.filterIndexed { index, _ -> index != i }
        if (isSafeReport(modifiedLevels)) return true
    }
    return false
}


import java.io.File

def safeReportCount = 0

new File("input.txt").eachLine { line ->
    def levels = line.split("\\s+").collect { it.toInteger() }
    if (levels.size() >= 2) {
        def firstDiff = levels[1] - levels[0]
        if (firstDiff == 0) return
        def isIncreasing = firstDiff > 0
        boolean safe = true
        for (int i = 0; i < levels.size() - 1; i++) {
            def diff = levels[i + 1] - levels[i]
            if (diff == 0 || (isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0) || Math.abs(diff) < 1 || Math.abs(diff) > 3) {
                safe = false
                break
            }
        }
        if (safe) safeReportCount++
    }
}

println safeReportCount


import java.io.File

fun main() {
    val file = File("input.txt")
    if (!file.isFile) return
    val ranges = mutableListOf<Pair<Long, Long>>()
    file.forEachLine { raw ->
        val line = raw.trim()
        if (line.isEmpty()) return@forEachLine
        val parts = line.split("-")
        if (parts.size != 2) return@forEachLine
        var a = parts[0].trim().toLong()
        var b = parts[1].trim().toLong()
        if (a > b) {
            val t = a; a = b; b = t
        }
        ranges.add(a to b)
    }
    if (ranges.isEmpty()) {
        println("Total fresh IDs: 0")
        return
    }
    ranges.sortWith(compareBy<Pair<Long, Long>> { it.first }.thenBy { it.second })
    var total = 0L
    var curMin = ranges[0].first
    var curMax = ranges[0].second
    for (i in 1 until ranges.size) {
        val (mn, mx) = ranges[i]
        if (mn <= curMax) {
            if (mx > curMax) curMax = mx
        } else {
            total += curMax - curMin + 1
            curMin = mn
            curMax = mx
        }
    }
    total += curMax - curMin + 1
    println("Total fresh IDs: $total")
}

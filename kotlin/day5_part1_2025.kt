
import java.io.File

data class Range(var min: Long, var max: Long)

fun contains(ranges: List<Range>, x: Long): Boolean {
    var l = 0
    var r = ranges.size
    while (l < r) {
        val m = (l + r) ushr 1
        val cur = ranges[m]
        when {
            x < cur.min -> r = m
            x > cur.max -> l = m + 1
            else -> return true
        }
    }
    return false
}

fun main() {
    val lines = File("input.txt").readLines()
    val rawRanges = mutableListOf<Range>()
    var parsingRanges = true
    var freshCount = 0L
    for (raw in lines) {
        val s = raw.trim()
        if (s.isEmpty()) {
            parsingRanges = false
            continue
        }
        if (parsingRanges) {
            val parts = s.split('-')
            if (parts.size != 2) return
            val mn = parts[0].toLong()
            val mx = parts[1].toLong()
            rawRanges.add(Range(mn, mx))
        } else {
            val id = s.toLong()
            if (rawRanges.isNotEmpty() && contains(rawRanges, id)) freshCount++
        }
    }
    if (rawRanges.isNotEmpty()) {
        rawRanges.sortWith(compareBy<Range> { it.min }.thenBy { it.max })
        val merged = mutableListOf<Range>()
        for (r in rawRanges) {
            if (merged.isEmpty() || r.min > merged.last().max) {
                merged.add(Range(r.min, r.max))
            } else if (r.max > merged.last().max) {
                merged.last().max = r.max
            }
        }
        rawRanges.clear()
        rawRanges.addAll(merged)
    }
    // re‑check IDs now that ranges are merged
    freshCount = 0L
    var idsStarted = false
    for (raw in lines) {
        val s = raw.trim()
        if (s.isEmpty()) {
            idsStarted = true
            continue
        }
        if (idsStarted) {
            val id = s.toLong()
            if (contains(rawRanges, id)) freshCount++
        }
    }
    println("Number of fresh ingredients: $freshCount")
}

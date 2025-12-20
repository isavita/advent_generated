import java.io.File

fun calc(s: String): Int {
    for (d1 in 9 downTo 0) {
        val idx = s.indexOf('0' + d1)
        if (idx == -1 || idx == s.lastIndex) continue
        var max2 = -1
        for (i in idx + 1 until s.length) {
            val c = s[i]
            if (c in '0'..'9') {
                val v = c - '0'
                if (v > max2) {
                    max2 = v
                    if (max2 == 9) break
                }
            }
        }
        if (max2 != -1) return d1 * 10 + max2
    }
    return 0
}

fun main() {
    var total = 0
    File("input.txt").forEachLine { line ->
        total += calc(line)
    }
    println(total)
}
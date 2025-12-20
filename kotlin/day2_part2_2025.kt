
import java.io.File

private fun isInvalid(x: ULong): Boolean {
    val s = x.toString()
    val n = s.length
    if (n <= 1) return false
    for (p in 1..n / 2) {
        if (n % p != 0) continue
        val k = n / p
        if (k < 2) continue
        var ok = true
        for (i in p until n) {
            if (s[i] != s[i % p]) { ok = false; break }
        }
        if (ok) return true
    }
    return false
}

fun main() {
    val txt = File("input.txt").readText()
    var i = 0
    var sum = 0UL
    while (i < txt.length) {
        while (i < txt.length && (txt[i] == ' ' || txt[i] == '\n' || txt[i] == '\r' || txt[i] == '\t' || txt[i] == ',')) i++
        if (i >= txt.length) break
        val startA = i
        while (i < txt.length && txt[i].isDigit()) i++
        if (i >= txt.length || txt[i] != '-') break
        val a = txt.substring(startA, i).toULong()
        i++ // skip '-'
        val startB = i
        while (i < txt.length && txt[i].isDigit()) i++
        if (startB == i) break
        var b = txt.substring(startB, i).toULong()
        var lo = a
        var hi = b
        if (lo > hi) { lo = b; hi = a }
        var x = lo
        while (true) {
            if (isInvalid(x)) sum += x
            if (x == hi) break
            if (x == ULong.MAX_VALUE) break
            x = x + 1UL
        }
    }
    println(sum)
}

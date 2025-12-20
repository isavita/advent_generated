
import java.io.File
import java.math.BigInteger

fun isSeparator(col: Int, lines: List<String>): Boolean {
    for (ln in lines) {
        if (col < ln.length && !ln[col].isWhitespace()) return false
    }
    return true
}

fun blockResult(sc: Int, ec: Int, lines: List<String>): BigInteger {
    val numbers = mutableListOf<BigInteger>()
    var op = 0          // 0 = none, 1 = +, 2 = *
    for (ln in lines) {
        val end = (ec + 1).coerceAtMost(ln.length)
        if (sc >= ln.length) continue
        var seg = ln.substring(sc, end)
        seg = seg.trim()
        if (seg.isEmpty()) continue
        when (seg) {
            "+" -> op = 1
            "*" -> op = 2
            else -> numbers.add(BigInteger(seg))
        }
    }
    if (numbers.isEmpty()) return BigInteger.ZERO
    return when (op) {
        1 -> numbers.reduce { acc, v -> acc + v }
        2 -> numbers.reduce { acc, v -> acc * v }
        else -> numbers.first()
    }
}

fun main() {
    val lines = File("input.txt").readLines()
    var maxW = 0
    for (ln in lines) if (ln.length > maxW) maxW = ln.length

    var grand = BigInteger.ZERO
    var inBlock = false
    var start = 0

    for (x in 0 until maxW) {
        if (!isSeparator(x, lines)) {
            if (!inBlock) {
                inBlock = true
                start = x
            }
        } else {
            if (inBlock) {
                grand += blockResult(start, x - 1, lines)
                inBlock = false
            }
        }
    }
    if (inBlock) grand += blockResult(start, maxW - 1, lines)

    println("Grand total: $grand")
}

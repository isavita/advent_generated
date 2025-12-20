
import java.io.File
import java.math.BigInteger
import java.util.*

// ---------- helpers ----------
private val ZERO = BigInteger.ZERO
private val ONE = BigInteger.ONE
private val TEN = BigInteger.TEN

private fun strToBig(s: String) = BigInteger(s)

// ---------- main ----------
fun main() {
    // read whole file, replace any whitespace with commas
    val raw = File("input.txt").readText()
    val cleaned = raw.map { if (it.isWhitespace()) ',' else it }.joinToString("")
    val tokens = cleaned.split(',').filter { it.isNotEmpty() }

    // pre‑compute 10^i for i = 0 .. 20
    val pow10 = Array(21) { BigInteger.ONE }
    for (i in 1..20) pow10[i] = pow10[i - 1].multiply(TEN)

    val ids = HashSet<BigInteger>()

    for (token in tokens) {
        val dash = token.indexOf('-')
        if (dash == -1) continue          // malformed token – ignore
        val start = strToBig(token.substring(0, dash))
        val end = strToBig(token.substring(dash + 1))
        val (lo, hi) = if (start <= end) start to end else end to start

        for (k in 1..10) {
            val multiplier = pow10[k].add(ONE)               // 10^k + 1
            val minSeed = pow10[k - 1]                       // 10^{k‑1}
            val maxSeed = pow10[k].subtract(ONE)             // 10^k – 1

            // sMin = ceil(lo / multiplier)
            var sMin = lo.add(multiplier).subtract(ONE).divide(multiplier)
            // sMax = floor(hi / multiplier)
            var sMax = hi.divide(multiplier)

            if (sMin < minSeed) sMin = minSeed
            if (sMax > maxSeed) sMax = maxSeed
            if (sMin > sMax) continue

            // iterate seeds – the amount is limited by the input size;
            // for very large intervals this loop could be replaced by a formula.
            var seed = sMin
            while (seed <= sMax) {
                val id = seed.multiply(multiplier)   // seed * (10^k + 1)
                ids.add(id)
                seed = seed.add(ONE)
            }
        }
    }

    var sum = ZERO
    for (v in ids) sum = sum.add(v)
    println(sum.toString())
}

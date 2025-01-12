
import java.io.File
import java.lang.Integer.min

const val MOD = 1 shl 24
const val NUM_STEPS = 2000
const val PATTERN_COUNT = 19 * 19 * 19 * 19

fun nextSecret(s: Long): Long {
    var x = s * 64
    var temp = s xor x
    temp = temp and (MOD - 1).toLong()
    x = temp / 32
    temp = temp xor x
    temp = temp and (MOD - 1).toLong()
    x = temp * 2048
    temp = temp xor x
    return temp and (MOD - 1).toLong()
}

fun encodeChange4(c1: Int, c2: Int, c3: Int, c4: Int): Int {
    return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19
}

fun main() {
    val initials = File("input.txt").readLines().filter { it.isNotBlank() }.map { it.toLong() }

    val buyers = initials.map { initVal ->
        val prices = IntArray(NUM_STEPS + 1)
        var s = initVal
        for (j in 0..NUM_STEPS) {
            prices[j] = (s % 10).toInt()
            if (j < NUM_STEPS) {
                s = nextSecret(s)
            }
        }
        val changes = IntArray(NUM_STEPS) { prices[it + 1] - prices[it] }
        Pair(prices, changes)
    }

    val globalSum = LongArray(PATTERN_COUNT)

    for ((prices, changes) in buyers) {
        val localPrice = IntArray(PATTERN_COUNT) { -1 }
        for (i in 0 until NUM_STEPS - 3) {
            val c1 = changes[i]
            val c2 = changes[i + 1]
            val c3 = changes[i + 2]
            val c4 = changes[i + 3]

            if (c1 < -9 || c1 > 9 || c2 < -9 || c2 > 9 || c3 < -9 || c3 > 9 || c4 < -9 || c4 > 9) {
                continue
            }
            val idx = encodeChange4(c1, c2, c3, c4)
            if (localPrice[idx] < 0) {
                localPrice[idx] = prices[i + 4]
            }
        }
        for (idx in 0 until PATTERN_COUNT) {
            val p = localPrice[idx]
            if (p >= 0) {
                globalSum[idx] += p.toLong()
            }
        }
    }

    println(globalSum.maxOrNull() ?: 0)
}

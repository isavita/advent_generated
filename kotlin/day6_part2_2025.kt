
import java.io.File
import java.math.BigInteger

fun main() {
    val lines = File("input.txt").readLines()
    if (lines.isEmpty()) {
        println("Grand total: 0")
        return
    }

    val maxW = lines.maxOf { it.length }
    val isSep = BooleanArray(maxW) { col ->
        var allSpace = true
        for (ln in lines) {
            if (col < ln.length && !ln[col].isWhitespace()) {
                allSpace = false
                break
            }
        }
        allSpace
    }

    var grandTotal = BigInteger.ZERO
    var inBlock = false
    var blockOp = '+'
    val blockNumbers = mutableListOf<BigInteger>()

    fun finishBlock() {
        if (blockNumbers.isEmpty()) return
        var blockRes = if (blockOp == '*') BigInteger.ONE else BigInteger.ZERO
        for (n in blockNumbers) {
            blockRes = if (blockOp == '*') blockRes.multiply(n) else blockRes.add(n)
        }
        grandTotal = grandTotal.add(blockRes)
        blockNumbers.clear()
    }

    for (x in 0 until maxW) {
        if (!isSep[x]) {
            if (!inBlock) {
                inBlock = true
                blockOp = '+'
                blockNumbers.clear()
            }

            val sb = StringBuilder()
            for (ln in lines) {
                if (x < ln.length) {
                    val ch = ln[x]
                    when {
                        ch.isDigit() -> sb.append(ch)
                        ch == '+' || ch == '*' -> blockOp = ch
                    }
                }
            }
            if (sb.isNotEmpty()) {
                blockNumbers.add(BigInteger(sb.toString()))
            }
        } else if (inBlock) {
            finishBlock()
            inBlock = false
        }
    }
    if (inBlock) finishBlock()

    println("Grand total: $grandTotal")
}

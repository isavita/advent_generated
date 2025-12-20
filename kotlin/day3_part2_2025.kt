
import java.io.File
import java.math.BigInteger

private const val TARGET = 12

fun main() {
    var total = BigInteger.ZERO
    File("input.txt").forEachLine { line ->
        var end = line.length
        while (end > 0 && (line[end - 1] !in '0'..'9')) end--
        if (end < TARGET) return@forEachLine
        val s = line.substring(0, end)

        val stack = CharArray(s.length)
        var size = 0
        var rem = s.length - TARGET

        for (c in s) {
            while (rem > 0 && size > 0 && stack[size - 1] < c) {
                size--
                rem--
            }
            stack[size++] = c
        }

        val numStr = String(stack, 0, TARGET)
        total = total.add(BigInteger(numStr))
    }
    println(total)
}


import java.io.File
import kotlin.math.abs

fun main() {
    val file = File("input.txt")
    if (!file.isFile) {
        System.err.println("input.txt not found")
        return
    }

    // store coordinates in primitive int arrays for speed
    val xs = IntArray(10_000)
    val ys = IntArray(10_000)
    var n = 0

    file.forEachLine { line ->
        var i = 0
        val len = line.length
        while (i < len) {
            // skip whitespace
            while (i < len && line[i].isWhitespace()) i++
            if (i >= len) break

            // read x
            var sign = 1
            var num = 0
            if (line[i] == '-') { sign = -1; i++ }
            while (i < len && line[i].isDigit()) {
                num = num * 10 + (line[i] - '0')
                i++
            }
            val x = sign * num

            // expect ','
            while (i < len && line[i].isWhitespace()) i++
            if (i < len && line[i] == ',') i++ else break

            // read y
            sign = 1
            num = 0
            while (i < len && line[i].isWhitespace()) i++
            if (i < len && line[i] == '-') { sign = -1; i++ }
            while (i < len && line[i].isDigit()) {
                num = num * 10 + (line[i] - '0')
                i++
            }
            val y = sign * num

            xs[n] = x
            ys[n] = y
            n++
        }
    }

    var best = 0L
    for (i in 0 until n) {
        val xi = xs[i]
        val yi = ys[i]
        for (j in i until n) {
            val dx = (abs(xi - xs[j]).toLong() + 1L)
            val dy = (abs(yi - ys[j]).toLong() + 1L)
            val area = dx * dy
            if (area > best) best = area
        }
    }

    println(best)
}

import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()[0]
    val (start, end) = input.split("-").map { it.toInt() }

    var count = 0
    for (i in start..end) {
        val s = i.toString()
        if (hasDoubleAndIncreasingDigits(s)) {
            count++
        }
    }

    println(count)
}

fun hasDoubleAndIncreasingDigits(s: String): Boolean {
    var hasDouble = false
    for (i in 0 until s.length - 1) {
        if (s[i] == s[i + 1]) {
            hasDouble = true
        }
        if (s[i] > s[i + 1]) {
            return false
        }
    }
    return hasDouble
}
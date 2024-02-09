import java.io.File

fun main() {
    val input = File("input.txt").readText().trim().split(",").map { it.toInt() }
    val lastSeen = mutableMapOf<Int, Int>()
    var prevNum = 0

    for (i in input.indices) {
        lastSeen[input[i]] = i + 1
        prevNum = input[i]
    }

    for (i in input.size until 30000000) {
        val nextNum = if (prevNum !in lastSeen) 0 else i - lastSeen[prevNum]!!
        lastSeen[prevNum] = i
        prevNum = nextNum
    }

    println(prevNum)
}
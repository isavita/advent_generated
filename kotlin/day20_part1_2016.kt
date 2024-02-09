import java.io.File

fun main() {
    val blockedIPs = mutableListOf<Pair<Long, Long>>()
    File("input.txt").readLines().forEach {
        val range = it.split("-")
        blockedIPs.add(Pair(range[0].toLong(), range[1].toLong()))
    }

    var currentIP: Long = 0
    blockedIPs.sortedBy { it.first }.forEach { range ->
        if (currentIP < range.first) {
            println(currentIP)
            return
        }
        currentIP = range.second + 1
    }
    println(currentIP)
}
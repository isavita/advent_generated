import java.io.File

fun calculateWaysToWinLongRace(time: Long, record: Long): Int {
    var waysToWin = 0
    for (holdTime in 1 until time) {
        val travelTime = time - holdTime
        val distance = holdTime * travelTime
        if (distance > record) {
            waysToWin++
        }
    }
    return waysToWin
}

fun main() {
    val file = File("input.txt")
    var time = 0L
    var distance = 0L

    file.forEachLine { line ->
        if (line.isNotEmpty()) {
            val parts = line.split(":")
            val lineNumber = parts[1].replace(" ", "")
            if (time == 0L) {
                time = lineNumber.toLong()
            } else {
                distance = lineNumber.toLong()
            }
        }
    }

    val waysToWin = calculateWaysToWinLongRace(time, distance)
    println(waysToWin)
}
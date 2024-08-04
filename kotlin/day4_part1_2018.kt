import java.io.File
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

data class Record(val timestamp: LocalDateTime, val event: String)

fun main() {
    val input = File("input.txt").readLines()
    val records = input.map { line ->
        val timestamp = line.substring(1, 17)
        val event = line.substring(19)
        Record(LocalDateTime.parse(timestamp, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")), event)
    }.sortedBy { it.timestamp }

    val guardSleepMinutes = mutableMapOf<Int, IntArray>()
    var currentGuardId = -1
    var sleepStart = 0

    for (record in records) {
        if (record.event.startsWith("Guard")) {
            currentGuardId = record.event.split(" ")[1].substring(1).toInt()
        } else if (record.event == "falls asleep") {
            sleepStart = record.timestamp.minute
        } else if (record.event == "wakes up") {
            val sleepEnd = record.timestamp.minute
            if (!guardSleepMinutes.containsKey(currentGuardId)) {
                guardSleepMinutes[currentGuardId] = IntArray(60)
            }
            for (minute in sleepStart until sleepEnd) {
                guardSleepMinutes[currentGuardId]!![minute]++
            }
        }
    }

    val (guardId, sleepMinutes) = guardSleepMinutes.maxByOrNull { it.value.sum() }!!
    val minute = sleepMinutes.indices.maxByOrNull { sleepMinutes[it] }!!

    println(guardId * minute)
}
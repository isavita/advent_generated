import java.io.File
import java.time.LocalDateTime

data class Record(val time: LocalDateTime, val event: String)

data class Guard(val id: Int, val minutes: IntArray, var totalMin: Int)

fun main(args: Array<String>) {
    val inputFile = File("input.txt")
    val records = mutableListOf<Record>()
    val guards = mutableMapOf<Int, Guard>()

    inputFile.forEachLine {
        val t = LocalDateTime.parse(it.substring(1, 17), java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
        records.add(Record(t, it.substring(19)))
    }

    records.sortBy { it.time }

    var currentGuard: Guard? = null
    var sleepStart = 0

    for (record in records) {
        when {
            record.event.contains("begins shift") -> {
                val id = record.event.split(" ")[1].substring(1).toInt()
                guards.putIfAbsent(id, Guard(id, IntArray(60), 0))
                currentGuard = guards[id]
            }
            record.event.contains("falls asleep") -> sleepStart = record.time.minute
            record.event.contains("wakes up") -> {
                for (i in sleepStart until record.time.minute) {
                    currentGuard?.minutes?.set(i, currentGuard.minutes[i] + 1)
                    currentGuard?.totalMin = currentGuard?.totalMin?.plus(1) ?: 1
                }
            }
        }
    }

    var mostFreqGuard: Guard? = null
    var mostFreqMin = 0

    for (g in guards.values) {
        for (i in g.minutes.indices) {
            if (mostFreqGuard == null || g.minutes[i] > mostFreqGuard.minutes[mostFreqMin]) {
                mostFreqGuard = g
                mostFreqMin = i
            }
        }
    }

    println(mostFreqGuard?.id?.times(mostFreqMin))
}
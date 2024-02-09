import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    val earliestDeparture = lines[0].toInt()
    val busIDs = lines[1].split(",")

    var earliestBusID = 0
    var minWaitTime = earliestDeparture

    for (id in busIDs) {
        if (id == "x") {
            continue
        }
        val busID = id.toInt()
        val waitTime = busID - (earliestDeparture % busID)
        if (waitTime < minWaitTime) {
            minWaitTime = waitTime
            earliestBusID = busID
        }
    }

    println(earliestBusID * minWaitTime)
}
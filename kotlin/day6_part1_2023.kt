import java.io.File

fun main() {
    // Read input from the file
    val input = File("input.txt").readText()

    // Split the input into lines and extract the times and distances
    val lines = input.split("\n")
    val times = lines[0].split("\\s+".toRegex()).drop(1).map { it.toLong() }
    val distances = lines[1].split("\\s+".toRegex()).drop(1).map { it.toLong() }

    // Calculate the number of ways to beat the record for each race
    val waysToWin = mutableListOf<Long>()
    for (i in times.indices) {
        val time = times[i]
        val recordDistance = distances[i]
        var count = 0L

        for (holdTime in 0..time) {
            val travelTime = time - holdTime
            val distance = holdTime * travelTime
            if (distance > recordDistance) {
                count++
            }
        }

        waysToWin.add(count)
    }

    // Multiply the number of ways to win each race
    val result = waysToWin.reduce { acc, num -> acc * num }

    // Print the result
    println(result)
}
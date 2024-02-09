import java.io.File

fun main(args: Array<String>) {
    val frequencyChanges = File("input.txt").readLines()
    val frequencies = mutableSetOf(0)
    var currentFrequency = 0

    while (true) {
        for (change in frequencyChanges) {
            val frequencyDelta = change.toInt()
            currentFrequency += frequencyDelta
            if (currentFrequency in frequencies) {
                println(currentFrequency)
                return
            }
            frequencies.add(currentFrequency)
        }
    }
}
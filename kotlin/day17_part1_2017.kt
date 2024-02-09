import java.io.File

fun main(args: Array<String>) {
    val steps = File("input.txt").readText().trim().toInt()
    val buffer = mutableListOf(0)
    var currentPos = 0

    for (i in 1..2017) {
        currentPos = (currentPos + steps) % buffer.size
        buffer.add(currentPos + 1, i)
        currentPos++
    }

    val result = buffer[(buffer.indexOf(2017) + 1) % buffer.size]
    println(result)
}
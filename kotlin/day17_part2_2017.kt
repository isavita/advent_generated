import java.io.File

fun main(args: Array<String>) {
    val steps = File("input.txt").readText().trim().toInt()
    var currentPos = 0
    var valueAfterZero = 0

    for (i in 1..50000000) {
        currentPos = (currentPos + steps) % i
        if (currentPos == 0) {
            valueAfterZero = i
        }
        currentPos++
    }

    println(valueAfterZero)
}
import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()

    var horizontalPosition = 0
    var depth = 0
    var aim = 0

    for (line in lines) {
        val command = line.split(" ")
        val direction = command[0]
        val units = command[1].toInt()

        when (direction) {
            "forward" -> {
                horizontalPosition += units
                depth += aim * units
            }
            "down" -> aim += units
            "up" -> aim -= units
        }
    }

    val product = horizontalPosition * depth
    println(product)
}
import java.io.File

fun main(args: Array<String>) {
    val lines = File("input.txt").readLines()
    
    var horizontalPosition = 0
    var depth = 0
    
    for (line in lines) {
        val command = line.split(" ")
        val direction = command[0]
        val units = command[1].toInt()

        when (direction) {
            "forward" -> horizontalPosition += units
            "down" -> depth += units
            "up" -> depth -= units
        }
    }

    val product = horizontalPosition * depth
    println(product)
}
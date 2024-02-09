import java.io.File

data class Ship(var x: Int, var y: Int, var facing: Int)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    val ship = Ship(0, 0, 0)
    lines.forEach { line ->
        val action = line[0]
        val value = line.substring(1).toInt()
        ship.processInstruction(action, value)
    }

    val manhattanDistance = Math.abs(ship.x) + Math.abs(ship.y)
    println(manhattanDistance)
}

fun Ship.processInstruction(action: Char, value: Int) {
    when (action) {
        'N' -> y += value
        'S' -> y -= value
        'E' -> x += value
        'W' -> x -= value
        'L' -> facing = (facing - value + 360) % 360
        'R' -> facing = (facing + value) % 360
        'F' -> when (facing) {
            0 -> x += value
            90 -> y -= value
            180 -> x -= value
            270 -> y += value
        }
    }
}
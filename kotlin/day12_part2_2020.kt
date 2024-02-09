import java.io.File

data class Ship(var x: Int, var y: Int, var waypointX: Int, var waypointY: Int)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val ship = Ship(0, 0, 10, 1)
    file.forEachLine {
        val action = it[0]
        val value = it.substring(1).toInt()
        ship.processInstruction(action, value)
    }

    val manhattanDistance = Math.abs(ship.x) + Math.abs(ship.y)
    println(manhattanDistance)
}

fun Ship.processInstruction(action: Char, value: Int) {
    when (action) {
        'N' -> waypointY += value
        'S' -> waypointY -= value
        'E' -> waypointX += value
        'W' -> waypointX -= value
        'L' -> rotateWaypoint(-value)
        'R' -> rotateWaypoint(value)
        'F' -> {
            x += waypointX * value
            y += waypointY * value
        }
    }
}

fun Ship.rotateWaypoint(degrees: Int) {
    var degrees = (degrees + 360) % 360
    when (degrees) {
        90, -270 -> {
            waypointX = waypointY.also { waypointY = -waypointX }
        }
        180, -180 -> {
            waypointX = -waypointX
            waypointY = -waypointY
        }
        270, -90 -> {
            waypointX = -waypointY.also { waypointY = waypointX }
        }
    }
}
import java.io.File

data class Position(val x: Int, val y: Int)

fun main() {
    val diagram = File("input.txt").readLines()
    val letters = mutableListOf<Char>()
    var steps = 0

    // Find the starting position
    var pos = Position(diagram[0].indexOf('|'), 0)
    var direction = Direction.DOWN

    while (true) {
        steps++
        val currentChar = diagram[pos.y][pos.x]

        if (currentChar in 'A'..'Z') {
            letters.add(currentChar)
        } else if (currentChar == '+') {
            // Change direction if needed
            direction = when (direction) {
                Direction.UP, Direction.DOWN -> if (pos.x > 0 && diagram[pos.y][pos.x - 1] != ' ') Direction.LEFT else Direction.RIGHT
                Direction.LEFT, Direction.RIGHT -> if (pos.y > 0 && diagram[pos.y - 1][pos.x] != ' ') Direction.UP else Direction.DOWN
            }
        }

        // Move to the next position
        pos = when (direction) {
            Direction.UP -> Position(pos.x, pos.y - 1)
            Direction.DOWN -> Position(pos.x, pos.y + 1)
            Direction.LEFT -> Position(pos.x - 1, pos.y)
            Direction.RIGHT -> Position(pos.x + 1, pos.y)
        }

        // Check if we've reached the end
        if (pos.y >= diagram.size || pos.x >= diagram[pos.y].length || diagram[pos.y][pos.x] == ' ') {
            break
        }
    }

    println("Letters seen: ${letters.joinToString("")}")
    println("Total steps: $steps")
}

enum class Direction {
    UP, DOWN, LEFT, RIGHT
}
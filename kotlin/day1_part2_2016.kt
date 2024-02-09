import java.io.File

data class Position(val x: Int, val y: Int)

fun main(args: Array<String>) {
    val instructions = File("input.txt").readLines().flatMap { it.split(", ") }
    println(firstRevisitedDistance(instructions))
}

fun firstRevisitedDistance(instructions: List<String>): Int {
    var pos = Position(0, 0)
    val visited = mutableSetOf<Position>()
    visited.add(pos)
    val directions = arrayOf(Position(0, 1), Position(1, 0), Position(0, -1), Position(-1, 0))
    var dirIndex = 0

    for (instruction in instructions) {
        val turn = instruction[0]
        val blocks = instruction.substring(1).toInt()

        if (turn == 'R') {
            dirIndex = (dirIndex + 1) % 4
        } else {
            dirIndex = (dirIndex - 1 + 4) % 4
        }

        repeat(blocks) {
            pos = Position(pos.x + directions[dirIndex].x, pos.y + directions[dirIndex].y)

            if (pos in visited) {
                return abs(pos.x) + abs(pos.y)
            }
            visited.add(pos)
        }
    }

    return -1
}

fun abs(x: Int): Int {
    return if (x < 0) -x else x
}
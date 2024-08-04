import java.io.File

data class Point(val x: Int, val y: Int)

enum class Direction {
    UP, DOWN, LEFT, RIGHT;

    fun move(point: Point): Point {
        return when (this) {
            UP -> Point(point.x, point.y - 1)
            DOWN -> Point(point.x, point.y + 1)
            LEFT -> Point(point.x - 1, point.y)
            RIGHT -> Point(point.x + 1, point.y)
        }
    }
}

fun main() {
    val input = File("input.txt").readLines()
    val grid = input.map { it.toCharArray() }

    val start = Point(input[0].indexOfFirst { it == '|' }, 0)
    val path = mutableListOf<Char>()
    var current = start
    var direction = Direction.DOWN

    while (true) {
        current = direction.move(current)
        val char = grid[current.y][current.x]

        if (char == ' ') break

        if (char in 'A'..'Z') {
            path.add(char)
        } else if (char == '+') {
            direction = when (direction) {
                Direction.UP, Direction.DOWN -> {
                    if (current.x > 0 && grid[current.y][current.x - 1] != ' ') Direction.LEFT
                    else Direction.RIGHT
                }
                Direction.LEFT, Direction.RIGHT -> {
                    if (current.y > 0 && grid[current.y - 1][current.x] != ' ') Direction.UP
                    else Direction.DOWN
                }
            }
        }
    }

    println(path.joinToString(""))
}
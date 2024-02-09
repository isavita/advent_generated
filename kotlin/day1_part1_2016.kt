import java.io.File

fun main() {
    val input = File("input.txt").readText().split(", ")
    var x = 0
    var y = 0
    var direction = 0

    for (move in input) {
        val turn = move[0]
        val distance = move.substring(1).toInt()

        if (turn == 'R') {
            direction = (direction + 1) % 4
        } else {
            direction = (direction + 3) % 4
        }

        when (direction) {
            0 -> y += distance
            1 -> x += distance
            2 -> y -= distance
            3 -> x -= distance
        }
    }

    println(Math.abs(x) + Math.abs(y))
}
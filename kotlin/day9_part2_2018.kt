
import java.io.File
import java.util.ArrayDeque

fun main() {
    val data = File("input.txt").readText().trim().split(" ")
    val players = data[0].toInt()
    val lastMarble = data[6].toInt() * 100

    val circle = ArrayDeque<Int>()
    circle.add(0)
    val scores = LongArray(players)
    var currentPlayer = 0

    for (marble in 1..lastMarble) {
        if (marble % 23 == 0) {
            repeat(7) { circle.addFirst(circle.removeLast()) }
            scores[currentPlayer] += marble.toLong() + circle.removeLast().toLong()
            circle.addLast(circle.removeFirst())
        } else {
            circle.addLast(circle.removeFirst())
            circle.addLast(marble)
        }
        currentPlayer = (currentPlayer + 1) % players
    }
    println(scores.maxOrNull())
}

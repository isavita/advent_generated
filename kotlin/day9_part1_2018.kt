import java.io.File

data class Marble(var value: Int, var prev: Marble? = null, var next: Marble? = null)

fun main(args: Array<String>) {
    val (players, lastMarble) = readInput("input.txt")
    println(playMarbleGame(players, lastMarble))
}

fun readInput(filename: String): Pair<Int, Int> {
    val lines = File(filename).readLines()
    val parts = lines[0].split(" ")
    val players = parts[0].toInt()
    val lastMarble = parts[6].toInt()
    return Pair(players, lastMarble)
}

fun playMarbleGame(players: Int, lastMarble: Int): Int {
    val scores = IntArray(players)
    var current = Marble(0)
    current.next = current
    current.prev = current

    for (marble in 1..lastMarble) {
        if (marble % 23 == 0) {
            val player = marble % players
            repeat(7) { current = current.prev!! }
            scores[player] += marble + current.value
            current.prev!!.next = current.next
            current.next!!.prev = current.prev
            current = current.next!!
        } else {
            current = current.next!!
            val newMarble = Marble(marble, current, current.next)
            current.next!!.prev = newMarble
            current.next = newMarble
            current = newMarble
        }
    }

    var maxScore = 0
    scores.forEach { score ->
        if (score > maxScore) {
            maxScore = score
        }
    }
    return maxScore
}
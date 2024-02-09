import java.io.File

fun main() {
    val input = File("input.txt").readText().trim().split("\n\n")
    val player1 = input[0].split("\n").drop(1).map { it.toInt() }.toMutableList()
    val player2 = input[1].split("\n").drop(1).map { it.toInt() }.toMutableList()

    while (player1.isNotEmpty() && player2.isNotEmpty()) {
        val card1 = player1.removeAt(0)
        val card2 = player2.removeAt(0)

        if (card1 > card2) {
            player1.add(card1)
            player1.add(card2)
        } else {
            player2.add(card2)
            player2.add(card1)
        }
    }

    val winningDeck = if (player1.isNotEmpty()) player1 else player2
    val score = winningDeck.reversed().mapIndexed { index, value -> value * (index + 1) }.sum()

    println(score)
}
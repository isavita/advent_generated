import java.io.File

fun main(args: Array<String>) {
    val player1Deck = mutableListOf<Int>()
    val player2Deck = mutableListOf<Int>()
    var currentDeck = player1Deck

    File("input.txt").forEachLine { line ->
        if (line == "") {
            currentDeck = player2Deck
        } else if (!line.contains("Player")) {
            currentDeck.add(line.toInt())
        }
    }

    val (winningDeck, _) = playRecursiveCombat(player1Deck, player2Deck)

    val score = winningDeck.foldIndexed(0) { index, acc, card ->
        acc + card * (winningDeck.size - index)
    }

    println(score)
}

fun playRecursiveCombat(player1: MutableList<Int>, player2: MutableList<Int>): Pair<List<Int>, List<Int>> {
    val previousRounds = mutableSetOf<String>()

    while (player1.isNotEmpty() && player2.isNotEmpty()) {
        val roundKey = "${player1.joinToString(",")}|${player2.joinToString(",")}"
        if (previousRounds.contains(roundKey)) {
            return Pair(player1, emptyList())
        }
        previousRounds.add(roundKey)

        val card1 = player1.removeAt(0)
        val card2 = player2.removeAt(0)

        if (player1.size >= card1 && player2.size >= card2) {
            val (subPlayer1, _) = playRecursiveCombat(player1.toMutableList().take(card1).toMutableList(), player2.toMutableList().take(card2).toMutableList())
            if (subPlayer1.isNotEmpty()) {
                player1.addAll(listOf(card1, card2))
            } else {
                player2.addAll(listOf(card2, card1))
            }
        } else {
            if (card1 > card2) {
                player1.addAll(listOf(card1, card2))
            } else {
                player2.addAll(listOf(card2, card1))
            }
        }
    }

    return Pair(player1, player2)
}
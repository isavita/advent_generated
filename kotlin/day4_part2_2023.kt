import java.io.File

data class Card(
    val winnings: Map<String, Int>,
    val givens: Map<String, Int>,
    var totalCount: Int
)

fun getPointsForCard(card: Card): Int {
    var points = 0
    for ((given, count) in card.givens) {
        if (card.winnings.containsKey(given)) {
            points += count * card.winnings[given]!!
        }
    }
    return points
}

fun lexLineIntoCard(line: String): Card {
    val cardDataStr = line.split(": ")[1]
    val cardData = cardDataStr.split(" | ")

    val re = Regex("[0-9]{1,2}")

    val winnings = mutableMapOf<String, Int>()
    re.findAll(cardData[0]).forEach { point ->
        winnings[point.value] = (winnings[point.value] ?: 0) + 1
    }

    val givens = mutableMapOf<String, Int>()
    re.findAll(cardData[1]).forEach { point ->
        givens[point.value] = (givens[point.value] ?: 0) + 1
    }

    return Card(winnings, givens, 1)
}

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    val cards = input.split("\n").mapNotNull { line ->
        if (line.isEmpty()) null
        else lexLineIntoCard(line)
    }

    cards.forEachIndexed { i, card ->
        val points = getPointsForCard(card)

        for (j in 1..points) {
            cards.getOrNull(i + j)?.totalCount = (cards.getOrNull(i + j)?.totalCount ?: 0) + card.totalCount
        }
    }

    val totalCards = cards.sumBy { it.totalCount }
    println(totalCards)
}
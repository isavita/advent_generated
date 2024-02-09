
import java.io.File

const val HighCard = 1
const val OnePair = 2
const val TwoPair = 3
const val ThreeKind = 4
const val FullHouse = 5
const val FourKind = 6
const val FiveKind = 7

data class Hand(val cards: String, val bid: Int)

data class RankedHand(val hand: Hand, val rank: Int)

val matches = Array(7) { mutableListOf<Hand>() }

fun findMatches(hands: List<Hand>) {
    for (hand in hands) {
        val count = mutableMapOf<Char, Int>()

        for (card in hand.cards) {
            count[card] = count.getOrDefault(card, 0) + 1
        }

        var value = 1
        for (c in count.values) {
            value *= c
        }

        when (value) {
            1 -> matches[6].add(hand)
            2 -> matches[5].add(hand)
            3 -> matches[3].add(hand)
            4 -> if (count.size == 2) matches[1].add(hand) else matches[4].add(hand)
            5 -> matches[0].add(hand)
            6 -> matches[2].add(hand)
            else -> println("oh no")
        }
    }
}

fun convertAndOrderMatches(): List<RankedHand> {
    val convertedMatches = mutableListOf<RankedHand>()

    for (category in matches) {
        val temp = mutableListOf<RankedHand>()

        for (hand in category) {
            var cards = hand.cards.replace("A", "E")
            cards = cards.replace("T", "A")
            cards = cards.replace("J", "B")
            cards = cards.replace("Q", "C")
            cards = cards.replace("K", "D")

            val num = cards.toInt(16)

            temp.add(RankedHand(hand, num))
        }

        temp.sortByDescending { it.rank }

        convertedMatches.addAll(temp)
    }

    return convertedMatches
}

fun main() {
    val lines = File("input.txt").readText()
    val hands = mutableListOf<Hand>()

    val re = Regex("[\\dAKQJT]+")
    val bidRe = Regex(" [\\d]+")

    for (line in lines.split("\n")) {
        if (line.isEmpty()) {
            continue
        }

        val cards = re.find(line)!!.value
        val bid = bidRe.find(line)!!.value.trim().toInt()

        hands.add(Hand(cards, bid))
    }

    findMatches(hands)

    val convertedMatches = convertAndOrderMatches()

    var total = 0
    for (i in convertedMatches.indices) {
        total += convertedMatches[i].hand.bid * (convertedMatches.size - i)
    }

    println(total)
}

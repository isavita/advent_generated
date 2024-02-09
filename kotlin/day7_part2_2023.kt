import java.io.File

data class Hand(val cards: String, val bid: Int)

val valueDict = mapOf('J' to 1, '2' to 2, '3' to 3, '4' to 4, '5' to 5, '6' to 6, '7' to 7, '8' to 8, '9' to 9, 'T' to 10, 'Q' to 11, 'K' to 12, 'A' to 13)

fun main(args: Array<String>) {
    val input = File("input.txt").readText()
    val lines = input.split("\n")

    val hands = mutableListOf<Hand>()

    val re = Regex("[\\dAKQJT]+")
    val bidRe = Regex(" [\\d]+")

    for (line in lines) {
        if (line.isEmpty()) {
            continue
        }

        val cards = re.find(line)?.value ?: ""
        val bid = bidRe.find(line)?.value?.substring(1)?.toInt() ?: 0

        hands.add(Hand(cards, bid))
    }

    val matches = List(7) { mutableListOf<Hand>() }

    for (hand in hands) {
        val count = mutableMapOf<Char, Int>()

        for (i in hand.cards) {
            count[i] = count.getOrDefault(i, 0) + 1
        }

        if (count['J'] ?: 0 > 0) {
            var highV = 0
            var highKey = 'J'
            for (y in count.keys) {
                if (y != 'J') {
                    if (count[y] ?: 0 > highV) {
                        highKey = y
                        highV = count[y] ?: 0
                    } else if (count[y] == highV && valueDict[y] ?: 0 > valueDict[highKey] ?: 0) {
                        highKey = y
                    }
                }
            }
            if (highKey != 'J') {
                count[highKey] = (count[highKey] ?: 0) + (count['J'] ?: 0)
                count.remove('J')
            }
        }

        var value = 1
        for (i in count.values) {
            value *= i
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

    val convertedMatches = mutableListOf<List<Int>>()

    for (x in matches) {
        val temp = mutableListOf<List<Int>>()
        for (i in x) {
            var y = i.cards.replace("A", "E")
            y = y.replace("T", "A")
            y = y.replace("J", "1")
            y = y.replace("Q", "C")
            y = y.replace("K", "D")
            val valInt = y.toLong(16).toInt()
            temp.add(listOf(valInt, i.bid))
        }
        temp.sortByDescending { it[0] }
        convertedMatches.addAll(temp)
    }

    var total = 0
    for (x in convertedMatches.indices) {
        total += convertedMatches[x][1] * (convertedMatches.size - x)
    }

    println(total)
}
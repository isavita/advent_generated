
import java.io.File

const val totalCups = 1000000
const val totalMoves = 10000000

fun main() {
    val input = File("input.txt").readText().trim()
    val cups = IntArray(totalCups + 1)
    var lastCup = 0

    input.forEachIndexed { i, char ->
        val cup = char.digitToInt()
        if (i > 0) {
            cups[lastCup] = cup
        }
        lastCup = cup
    }

    for (i in input.length + 1..totalCups) {
        cups[lastCup] = i
        lastCup = i
    }
    cups[lastCup] = input[0].digitToInt()

    var currentCup = input[0].digitToInt()
    repeat(totalMoves) {
        val pickup1 = cups[currentCup]
        val pickup2 = cups[pickup1]
        val pickup3 = cups[pickup2]

        cups[currentCup] = cups[pickup3]

        var destinationCup = currentCup - 1
        if (destinationCup == 0) {
            destinationCup = totalCups
        }
        while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
            destinationCup--
            if (destinationCup == 0) {
                destinationCup = totalCups
            }
        }

        cups[pickup3] = cups[destinationCup]
        cups[destinationCup] = pickup1

        currentCup = cups[currentCup]
    }

    val cup1 = cups[1]
    val cup2 = cups[cup1]
    println(cup1.toLong() * cup2.toLong())
}

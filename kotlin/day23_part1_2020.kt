import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()

    val cups = IntArray(input.length + 1)
    var currentCup = 0

    for (i in input.indices) {
        val cup = input[i].toString().toInt()
        if (i == 0) {
            currentCup = cup
        }
        if (i < input.length - 1) {
            val nextCup = input[i + 1].toString().toInt()
            cups[cup] = nextCup
        }
    }
    val firstCup = input[0].toString().toInt()
    val lastCup = input[input.length - 1].toString().toInt()
    cups[lastCup] = firstCup

    repeat(100) {
        val pickup1 = cups[currentCup]
        val pickup2 = cups[pickup1]
        val pickup3 = cups[pickup2]

        cups[currentCup] = cups[pickup3]

        var destinationCup = currentCup - 1
        if (destinationCup < 1) {
            destinationCup = input.length
        }
        while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
            destinationCup--
            if (destinationCup < 1) {
                destinationCup = input.length
            }
        }

        cups[pickup3] = cups[destinationCup]
        cups[destinationCup] = pickup1

        currentCup = cups[currentCup]
    }

    var cup = cups[1]
    while (cup != 1) {
        print(cup)
        cup = cups[cup]
        if (cup == 1) {
            break
        }
    }
    println()
}
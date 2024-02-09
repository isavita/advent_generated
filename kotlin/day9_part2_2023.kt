
import java.io.File

fun parseInput(input: List<String>): List<List<Int>> {
    return input.map { line -> parseStringToInts(line) }
}

fun parseStringToInts(numbersLine: String): List<Int> {
    return numbersLine.split(" ").map { it.toInt() }
}

fun allZeros(nums: List<Int>): Boolean {
    return nums.all { it == 0 }
}

fun calculateExtrapolation(history: List<Int>): List<Int> {
    val extrapolations = mutableListOf<Int>()
    for (i in 1 until history.size) {
        val extrapolation = history[i] - history[i - 1]
        extrapolations.add(extrapolation)
    }
    return extrapolations
}

fun calculateExtrapolations(history: List<Int>): List<List<Int>> {
    val extrapolationsSeries = mutableListOf<List<Int>>()
    extrapolationsSeries.add(history)

    for (i in 1 until history.size) {
        val previousExtrapolations = extrapolationsSeries[i - 1]
        if (allZeros(previousExtrapolations)) {
            return extrapolationsSeries
        }

        val extrapolations = calculateExtrapolation(previousExtrapolations)
        extrapolationsSeries.add(extrapolations)
    }

    return extrapolationsSeries
}

fun solve(input: List<String>): Int {
    val histories = parseInput(input)
    var res = 0

    for (history in histories) {
        val extrapolationsSeries = calculateExtrapolations(history)

        var pastPrediction = 0
        for (i in extrapolationsSeries.size - 1 downTo 0) {
            pastPrediction = extrapolationsSeries[i][0] - pastPrediction
        }

        res += pastPrediction
    }

    return res
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}

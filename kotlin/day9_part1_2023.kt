import java.io.File

fun parseInput(input: List<String>): List<List<Int>> {
    return input.map { line ->
        line.split(" ").map { it.toInt() }
    }
}

fun allZeros(nums: List<Int>): Boolean {
    return nums.all { it == 0 }
}

fun calculateExtrapolation(history: List<Int>): List<Int> {
    val extrapolations = mutableListOf<Int>()
    for (i in 1 until history.size) {
        extrapolations.add(history[i] - history[i - 1])
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

        var futurePrediction = 0
        for (i in extrapolationsSeries.indices.reversed()) {
            futurePrediction = extrapolationsSeries[i][extrapolationsSeries[i].size - 1] + futurePrediction
        }

        res += futurePrediction
    }

    return res
}

fun main(args: Array<String>) {
    val input = File("input.txt").readLines()
    println(solve(input))
}
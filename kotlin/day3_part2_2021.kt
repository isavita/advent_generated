import java.io.File

fun main() {
    val input = File("input.txt").readLines()

    // Part 1: Calculate gamma and epsilon rates
    val gammaRate = calculateGammaRate(input)
    val epsilonRate = calculateEpsilonRate(input)

    val powerConsumption = gammaRate * epsilonRate
    println("Power Consumption: $powerConsumption")

    // Part 2: Calculate oxygen generator rating and CO2 scrubber rating
    val oxygenGeneratorRating = calculateRating(input, true)
    val co2ScrubberRating = calculateRating(input, false)

    val lifeSupportRating = oxygenGeneratorRating * co2ScrubberRating
    println("Life Support Rating: $lifeSupportRating")
}

fun calculateGammaRate(input: List<String>): Int {
    val bitCount = input[0].length
    val count = IntArray(bitCount)

    input.forEach { line ->
        line.forEachIndexed { index, char ->
            if (char == '1') count[index]++
        }
    }

    val gammaRateBinary = StringBuilder(bitCount)
    for (i in 0 until bitCount) {
        if (count[i] > input.size / 2) {
            gammaRateBinary.append('1')
        } else {
            gammaRateBinary.append('0')
        }
    }

    return gammaRateBinary.toString().toInt(2)
}

fun calculateEpsilonRate(input: List<String>): Int {
    val gammaRateBinary = calculateGammaRate(input).toString(2).padStart(input[0].length, '0')
    val epsilonRateBinary = StringBuilder(gammaRateBinary.length)

    for (char in gammaRateBinary) {
        epsilonRateBinary.append(if (char == '1') '0' else '1')
    }

    return epsilonRateBinary.toString().toInt(2)
}

fun calculateRating(input: List<String>, isOxygen: Boolean): Int {
    var filteredList = input.toMutableList()
    var position = 0

    while (filteredList.size > 1) {
        val count = filteredList.count { it[position] == '1' }
        val mostCommonBit = if (count >= (filteredList.size - count)) '1' else '0'
        val bitCriteria = if (isOxygen) mostCommonBit else if (mostCommonBit == '1') '0' else '1'

        filteredList = filteredList.filter { it[position] == bitCriteria }.toMutableList()
        position++
    }

    return filteredList[0].toInt(2)
}
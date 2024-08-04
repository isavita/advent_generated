import java.io.File
import java.util.PriorityQueue

data class Package(val weight: Int)

fun main() {
    val inputFile = File("input.txt")
    val weights = inputFile.readLines().map { it.toInt() }
    val totalWeight = weights.sum()

    if (totalWeight % 3 != 0) {
        println("It is not possible to divide the packages into three groups of equal weight.")
        return
    }

    val targetWeight = totalWeight / 3
    val minSize = findMinSize(weights, targetWeight)
    val minQuantumEntanglement = findMinQuantumEntanglement(weights, targetWeight, minSize)

    println("The quantum entanglement of the first group of packages in the ideal configuration is $minQuantumEntanglement.")
}

fun findMinSize(weights: List<Int>, targetWeight: Int): Int {
    for (size in 1..weights.size) {
        if (canFormGroup(weights, targetWeight, size)) {
            return size
        }
    }
    return Int.MAX_VALUE
}

fun canFormGroup(weights: List<Int>, targetWeight: Int, size: Int): Boolean {
    val combinations = generateCombinations(weights, size)
    for (combination in combinations) {
        if (combination.sum() == targetWeight) {
            return true
        }
    }
    return false
}

fun generateCombinations(weights: List<Int>, size: Int): List<List<Int>> {
    val combinations = mutableListOf<List<Int>>()
    generateCombinations(weights, size, 0, listOf(), combinations)
    return combinations
}

fun generateCombinations(weights: List<Int>, size: Int, start: Int, current: List<Int>, combinations: MutableList<List<Int>>) {
    if (current.size == size) {
        combinations.add(current)
        return
    }
    for (i in start until weights.size) {
        generateCombinations(weights, size, i + 1, current + weights[i], combinations)
    }
}

fun findMinQuantumEntanglement(weights: List<Int>, targetWeight: Int, size: Int): Long {
    val combinations = generateCombinations(weights, size)
    val validCombinations = combinations.filter { it.sum() == targetWeight }
    return validCombinations.minOfOrNull { it.fold(1L) { acc, weight -> acc * weight } } ?: Long.MAX_VALUE
}
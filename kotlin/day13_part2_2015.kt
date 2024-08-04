import java.io.File

data class Happiness(val person: String, val neighbor: String, val value: Int)

fun main() {
    val input = File("input.txt").readLines()
    val happinessList = mutableListOf<Happiness>()

    input.forEach { line ->
        val parts = line.split(" ")
        val person = parts[0]
        val neighbor = parts[10].replace(".", "")
        val value = if (parts[2] == "gain") parts[3].toInt() else -parts[3].toInt()
        happinessList.add(Happiness(person, neighbor, value))
    }

    val people = happinessList.map { it.person }.toSet()
    val happinessMap = mutableMapOf<Pair<String, String>, Int>()

    happinessList.forEach { happiness ->
        happinessMap[Pair(happiness.person, happiness.neighbor)] = happiness.value
    }

    val permutations = people.toList().permutations()
    var maxHappiness = Int.MIN_VALUE

    for (permutation in permutations) {
        var totalHappiness = 0
        for (i in permutation.indices) {
            val person = permutation[i]
            val nextPerson = permutation[(i + 1) % permutation.size]
            totalHappiness += happinessMap[Pair(person, nextPerson)] ?: 0
            totalHappiness += happinessMap[Pair(nextPerson, person)] ?: 0
        }
        maxHappiness = maxOf(maxHappiness, totalHappiness)
    }

    println("The total change in happiness for the optimal seating arrangement is $maxHappiness")

    // Part Two
    val myself = "You"
    val extendedPeople = people + myself
    val extendedHappinessMap = happinessMap.toMutableMap()

    extendedPeople.forEach { person ->
        extendedHappinessMap[Pair(person, myself)] = 0
        extendedHappinessMap[Pair(myself, person)] = 0
    }

    val extendedPermutations = extendedPeople.toList().permutations()
    maxHappiness = Int.MIN_VALUE

    for (permutation in extendedPermutations) {
        var totalHappiness = 0
        for (i in permutation.indices) {
            val person = permutation[i]
            val nextPerson = permutation[(i + 1) % permutation.size]
            totalHappiness += extendedHappinessMap[Pair(person, nextPerson)] ?: 0
            totalHappiness += extendedHappinessMap[Pair(nextPerson, person)] ?: 0
        }
        maxHappiness = maxOf(maxHappiness, totalHappiness)
    }

    println("The total change in happiness for the optimal seating arrangement including yourself is $maxHappiness")
}

fun <T> List<T>.permutations(): List<List<T>> {
    if (this.size == 1) return listOf(this)
    val perms = mutableListOf<List<T>>()
    val sub = this.drop(1)
    for (item in sub.permutations()) {
        for (i in 0..item.size) {
            val newPerm = item.take(i) + this.first() + item.drop(i)
            perms.add(newPerm)
        }
    }
    return perms
}
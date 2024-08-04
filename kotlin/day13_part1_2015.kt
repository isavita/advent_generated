import java.io.File

fun main() {
    val happinessMap = readHappinessValues("input.txt")
    val guests = happinessMap.keys.toList()
    val maxHappiness = calculateOptimalArrangement(guests, happinessMap)
    println(maxHappiness)
}

fun readHappinessValues(filename: String): MutableMap<String, MutableMap<String, Int>> {
    val happinessMap = mutableMapOf<String, MutableMap<String, Int>>()
    File(filename).forEachLine { line ->
        val parts = line.split(" ")
        if (parts.size < 11) return@forEachLine
        val from = parts[0]
        val to = parts[10].dropLast(1)
        var change = parts[3].toInt()
        if (parts[2] == "lose") change = -change

        happinessMap.computeIfAbsent(from) { mutableMapOf() }[to] = change
    }
    return happinessMap
}

fun calculateOptimalArrangement(guests: List<String>, happinessMap: Map<String, Map<String, Int>>): Int {
    var maxHappiness = 0
    permute(guests.toMutableList(), 0, { happiness -> if (happiness > maxHappiness) maxHappiness = happiness }, happinessMap)
    return maxHappiness
}

fun permute(arr: MutableList<String>, i: Int, updateMax: (Int) -> Unit, happinessMap: Map<String, Map<String, Int>>) {
    if (i == arr.size) {
        updateMax(calculateHappiness(arr, happinessMap))
        return
    }
    for (j in i until arr.size) {
        arr[i] = arr[j].also { arr[j] = arr[i] }
        permute(arr, i + 1, updateMax, happinessMap)
        arr[j] = arr[i].also { arr[i] = arr[j] }
    }
}

fun calculateHappiness(arrangement: List<String>, happinessMap: Map<String, Map<String, Int>>): Int {
    var happiness = 0
    val n = arrangement.size
    for (i in arrangement.indices) {
        val left = (i + n - 1) % n
        val right = (i + 1) % n
        happiness += happinessMap[arrangement[i]]?.get(arrangement[left]) ?: 0
        happiness += happinessMap[arrangement[i]]?.get(arrangement[right]) ?: 0
    }
    return happiness
}
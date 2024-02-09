import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val lines = file.readLines()

    var initialState = ""
    val rules = mutableMapOf<String, Char>()

    for (line in lines) {
        if (line.contains("initial state")) {
            initialState = line.split(": ")[1]
        } else if (line.contains("=>")) {
            val parts = line.split(" => ")
            rules[parts[0]] = parts[1][0]
        }
    }

    val state = mutableMapOf<Int, Char>()
    initialState.forEachIndexed { index, c ->
        if (c == '#') {
            state[index] = '#'
        }
    }

    var previousPattern = ""
    var previousSum = 0
    var offset = 0
    for (generation in 0 until 50000000000) {
        val newState = mutableMapOf<Int, Char>()
        val (minPot, maxPot) = minMaxKeys(state)
        for (i in minPot - 2..maxPot + 2) {
            var pattern = ""
            for (j in i - 2..i + 2) {
                pattern += if (state[j] == '#') '#' else '.'
            }
            if (rules[pattern] == '#') {
                newState[i] = '#'
            }
        }
        state.clear()
        state.putAll(newState)

        val (currentPattern, currentSum) = statePattern(state)
        if (currentPattern == previousPattern) {
            offset = currentSum - previousSum
            val remainingGenerations = 50000000000 - generation - 1
            val finalSum = currentSum + offset * remainingGenerations
            println(finalSum)
            return
        }
        previousPattern = currentPattern
        previousSum = currentSum
    }
}

fun minMaxKeys(m: Map<Int, Char>): Pair<Int, Int> {
    var minKey = 0
    var maxKey = 0
    var first = true
    for (k in m.keys) {
        if (first) {
            minKey = k
            maxKey = k
            first = false
        } else {
            if (k < minKey) {
                minKey = k
            }
            if (k > maxKey) {
                maxKey = k
            }
        }
    }
    return Pair(minKey, maxKey)
}

fun statePattern(m: Map<Int, Char>): Pair<String, Int> {
    val (minPot, maxPot) = minMaxKeys(m)
    val pattern = StringBuilder()
    var sum = 0
    for (i in minPot..maxPot) {
        pattern.append(if (m[i] == '#') '#' else '.')
        if (m[i] == '#') {
            sum += i
        }
    }
    return Pair(pattern.toString(), sum)
}
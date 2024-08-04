import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
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
    for ((i, c) in initialState.withIndex()) {
        if (c == '#') {
            state[i] = '#'
        }
    }

    for (generation in 0 until 20) {
        val newState = mutableMapOf<Int, Char>()
        val (minPot, maxPot) = minMaxKeys(state)
        for (i in minPot - 2..maxPot + 2) {
            var pattern = ""
            for (j in i - 2..i + 2) {
                pattern += state.getOrDefault(j, '.')
            }
            if (rules[pattern] == '#') {
                newState[i] = '#'
            }
        }
        state.clear()
        state.putAll(newState)
    }

    val sum = state.keys.sum()
    println(sum)
}

fun minMaxKeys(m: Map<Int, Char>): Pair<Int, Int> {
    var minKey = Int.MAX_VALUE
    var maxKey = Int.MIN_VALUE
    for (k in m.keys) {
        if (k < minKey) minKey = k
        if (k > maxKey) maxKey = k
    }
    return Pair(minKey, maxKey)
}
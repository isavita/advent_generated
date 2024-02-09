import java.io.File

fun main(args: Array<String>) {
    val (template, rules) = readInput("input.txt")
    val pairCounts = mutableMapOf<String, Long>()

    for (i in 0 until template.length - 1) {
        pairCounts[template.substring(i, i + 2)] = pairCounts.getOrDefault(template.substring(i, i + 2), 0) + 1
    }

    repeat(40) {
        val newPairCounts = mutableMapOf<String, Long>()
        pairCounts.forEach { (pair, count) ->
            if (rules.containsKey(pair)) {
                val insert = rules[pair]!!
                newPairCounts[pair[0] + insert] = newPairCounts.getOrDefault(pair[0] + insert, 0) + count
                newPairCounts[insert + pair[1]] = newPairCounts.getOrDefault(insert + pair[1], 0) + count
            } else {
                newPairCounts[pair] = newPairCounts.getOrDefault(pair, 0) + count
            }
        }
        pairCounts.clear()
        pairCounts.putAll(newPairCounts)
    }

    val elementCounts = mutableMapOf<Char, Long>()
    pairCounts.forEach { (pair, count) ->
        elementCounts[pair[0]] = elementCounts.getOrDefault(pair[0], 0) + count
    }
    elementCounts[template.last()] = elementCounts.getOrDefault(template.last(), 0) + 1

    var maxCount = 0L
    var minCount = Long.MAX_VALUE
    elementCounts.values.forEach { count ->
        if (count > maxCount) {
            maxCount = count
        }
        if (count < minCount) {
            minCount = count
        }
    }

    println(maxCount - minCount)
}

fun readInput(filename: String): Pair<String, Map<String, String>> {
    val lines = File(filename).readLines()
    val template = lines[0]
    val rules = mutableMapOf<String, String>()

    for (i in 1 until lines.size) {
        val line = lines[i]
        if (line.isNotEmpty()) {
            val parts = line.split(" -> ")
            rules[parts[0]] = parts[1]
        }
    }

    return Pair(template, rules)
}
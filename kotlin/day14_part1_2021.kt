import java.io.File

fun main(args: Array<String>) {
    val inputFile = File("input.txt")
    val lines = inputFile.readLines()
    var polymer = lines[0]
    val rules = mutableMapOf<String, String>()

    for (i in 1 until lines.size) {
        val line = lines[i]
        if (line.isBlank()) {
            continue
        }
        val parts = line.split(" -> ")
        rules[parts[0]] = parts[1]
    }

    repeat(10) {
        polymer = applyInsertion(polymer, rules)
    }

    val counts = countElements(polymer)
    val (min, max) = minMax(counts)

    println(max - min)
}

fun applyInsertion(polymer: String, rules: Map<String, String>): String {
    val newPolymer = StringBuilder()
    for (i in 0 until polymer.length - 1) {
        newPolymer.append(polymer[i])
        if (polymer.substring(i, i + 2) in rules) {
            newPolymer.append(rules[polymer.substring(i, i + 2)])
        }
    }
    newPolymer.append(polymer[polymer.length - 1])
    return newPolymer.toString()
}

fun countElements(polymer: String): Map<Char, Int> {
    val counts = mutableMapOf<Char, Int>()
    for (c in polymer) {
        counts[c] = counts.getOrDefault(c, 0) + 1
    }
    return counts
}

fun minMax(counts: Map<Char, Int>): Pair<Int, Int> {
    var min = Int.MAX_VALUE
    var max = Int.MIN_VALUE
    for (count in counts.values) {
        if (count < min) {
            min = count
        }
        if (count > max) {
            max = count
        }
    }
    return Pair(min, max)
}
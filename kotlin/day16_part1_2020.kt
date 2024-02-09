import java.io.File

data class Rule(val name: String, val ranges: List<IntRange>) {
    fun isValid(value: Int): Boolean {
        return ranges.any { value in it }
    }
}

fun main(args: Array<String>) {
    val rules = mutableListOf<Rule>()
    var scanningRules = true
    var errorRate = 0

    File("input.txt").forEachLine { line ->
        if (line.isBlank()) {
            return@forEachLine
        }
        if (line == "your ticket:" || line == "nearby tickets:") {
            scanningRules = false
            return@forEachLine
        }
        if (scanningRules) {
            val matchResult = Regex("""^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$""").find(line)
            if (matchResult != null) {
                val (name, range1Start, range1End, range2Start, range2End) = matchResult.destructured
                val ranges = listOf(IntRange(range1Start.toInt(), range1End.toInt()), IntRange(range2Start.toInt(), range2End.toInt()))
                rules.add(Rule(name, ranges))
            }
        } else {
            line.split(",").map { it.toInt() }.forEach { value ->
                if (!isValidForAnyRule(value, rules)) {
                    errorRate += value
                }
            }
        }
    }

    println(errorRate)
}

fun isValidForAnyRule(value: Int, rules: List<Rule>): Boolean {
    return rules.any { it.isValid(value) }
}
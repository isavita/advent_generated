import java.io.File

fun main(args: Array<String>) {
    val input = File("input.txt").readText().trim()
    val result = solve(input)
    println(result)
}

fun solve(input: String): Int {
    val (graph, messages) = parseInput(input)

    fillInGraph(graph, 42)
    fillInGraph(graph, 31)

    val part42 = "(${graph[42]!!.resolved.joinToString("|")})"
    val part31 = "(${graph[31]!!.resolved.joinToString("|")})"

    val rule8String = "($part42)+"

    fun makeRegexp(num: Int): Regex {
        return Regex("^$rule8String${part42.repeat(num)}${part31.repeat(num)}$")
    }

    var matchRuleZero = 0
    for (m in messages) {
        for (i in 1 until 10) {
            val pattern = makeRegexp(i)
            if (pattern.matches(m)) {
                matchRuleZero++
                break
            }
        }
    }

    return matchRuleZero
}

fun fillInGraph(graph: MutableMap<Int, Rule>, entry: Int): List<String> {
    if (graph[entry]!!.resolved.isNotEmpty()) {
        return graph[entry]!!.resolved.toList()
    }

    for (option in graph[entry]!!.options) {
        var resolved = listOf("")
        for (entryPoint in option) {
            val nestedResolveVals = fillInGraph(graph, entryPoint)
            var newResolved = mutableListOf<String>()
            for (nextPiece in nestedResolveVals) {
                for (prev in resolved) {
                    newResolved.add(prev + nextPiece)
                }
            }
            resolved = newResolved
        }
        graph[entry]!!.resolved.addAll(resolved)
    }

    return graph[entry]!!.resolved.toList()
}

data class Rule(
    var resolved: MutableList<String> = mutableListOf(),
    var options: List<List<Int>> = emptyList()
)

fun parseInput(input: String): Pair<MutableMap<Int, Rule>, List<String>> {
    val parts = input.split("\n\n")

    val rules = mutableMapOf<Int, Rule>()
    for (r in parts[0].split("\n")) {
        if (Regex("[a-z]").containsMatchIn(r)) {
            val (num, char) = """(\d+): "(\w)"""".toRegex().find(r)!!.destructured
            rules[num.toInt()] = Rule(mutableListOf(char))
        } else {
            val (key, ruleStrings) = r.split(": ")
            val ruleNum = key.toInt()
            val newRule = Rule()
            for (ruleNums in ruleStrings.split(" | ")) {
                val nums = ruleNums.split(" ").map { it.toInt() }
                newRule.options = newRule.options.plusElement(nums)
            }
            rules[ruleNum] = newRule
        }
    }

    val messages = parts[1].split("\n")

    return Pair(rules, messages)
}

fun toInt(s: String): Int {
    return s.toInt()
}
import java.io.File

fun main(args: Array<String>) {
    val rules = mutableMapOf<String, List<BagRule>>()
    val ruleRegex = Regex("(\\d+) (\\w+ \\w+) bags?[,.]")

    File("input.txt").forEachLine { line ->
        val parts = line.split(" bags contain ")
        val container = parts[0]
        val contents = parts[1]

        if (contents == "no other bags.") {
            return@forEachLine
        }

        ruleRegex.findAll(contents).forEach { match ->
            val count = match.groupValues[1].toInt()
            val color = match.groupValues[2]
            rules[container] = rules.getOrDefault(container, listOf()) + BagRule(color, count)
        }
    }

    val totalBags = countBags("shiny gold", rules) - 1
    println(totalBags)
}

data class BagRule(val color: String, val count: Int)

fun countBags(color: String, rules: Map<String, List<BagRule>>): Int {
    var count = 1
    rules[color]?.forEach { rule ->
        count += rule.count * countBags(rule.color, rules)
    }
    return count
}
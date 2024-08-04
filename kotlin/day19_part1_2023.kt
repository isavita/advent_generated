import java.io.File

data class Part(val x: Int, val m: Int, val a: Int, val s: Int)

data class Rule(val condition: String?, val destination: String)

data class Workflow(val name: String, val rules: List<Rule>)

fun parseInput(filename: String): Pair<Map<String, Workflow>, List<Part>> {
    val lines = File(filename).readLines()
    val workflows = mutableMapOf<String, Workflow>()
    var i = 0

    while (lines[i].isNotBlank()) {
        val line = lines[i]
        val name = line.substringBefore('{')
        val rules = line.substringAfter('{').substringBefore('}').split(',')
            .map { ruleStr ->
                val parts = ruleStr.split(':')
                if (parts.size == 1) {
                    Rule(null, parts[0])
                } else {
                    Rule(parts[0], parts[1])
                }
            }
        workflows[name] = Workflow(name, rules)
        i++
    }

    i++ // Skip the blank line

    val parts = mutableListOf<Part>()
    while (i < lines.size) {
        val line = lines[i]
        val ratings = line.substringAfter('{').substringBefore('}').split(',')
            .associate { ratingStr ->
                val (key, value) = ratingStr.split('=')
                key to value.toInt()
            }
        parts.add(Part(ratings["x"]!!, ratings["m"]!!, ratings["a"]!!, ratings["s"]!!))
        i++
    }

    return Pair(workflows, parts)
}

fun evaluateCondition(condition: String, part: Part): Boolean {
    val variable = condition[0]
    val operator = condition[1]
    val value = condition.substring(2).toInt()

    val partValue = when (variable) {
        'x' -> part.x
        'm' -> part.m
        'a' -> part.a
        's' -> part.s
        else -> throw IllegalArgumentException("Unknown variable: $variable")
    }

    return when (operator) {
        '>' -> partValue > value
        '<' -> partValue < value
        else -> throw IllegalArgumentException("Unknown operator: $operator")
    }
}

fun processPart(part: Part, workflows: Map<String, Workflow>): Boolean {
    var currentWorkflow = "in"

    while (currentWorkflow != "A" && currentWorkflow != "R") {
        val workflow = workflows[currentWorkflow]!!
        for (rule in workflow.rules) {
            if (rule.condition == null || evaluateCondition(rule.condition, part)) {
                currentWorkflow = rule.destination
                break
            }
        }
    }

    return currentWorkflow == "A"
}

fun main() {
    val (workflows, parts) = parseInput("input.txt")

    val acceptedParts = parts.filter { part -> processPart(part, workflows) }
    val sumOfRatings = acceptedParts.sumOf { part -> part.x + part.m + part.a + part.s }

    println(sumOfRatings)
}
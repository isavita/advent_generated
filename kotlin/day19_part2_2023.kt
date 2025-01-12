
import java.io.File

data class Rule(val category: Char, val operator: Char, val num: Int, val workflowName: String)

typealias Workflows = Map<String, List<Rule>>
typealias Part = Map<Char, Int>

data class Interval(var start: Int, var end: Int)
typealias PartInterval = MutableMap<Char, Interval>

fun parseInput(input: List<String>): Pair<Workflows, List<Part>> {
    val workflows = mutableMapOf<String, MutableList<Rule>>()
    val parts = mutableListOf<Part>()

    var i = 0
    while (input[i].isNotEmpty()) {
        val (workflowName, rules) = parseWorkflow(input[i])
        workflows[workflowName] = rules.toMutableList()
        i++
    }

    for (j in i + 1 until input.size) {
        parts.add(parsePart(input[j]))
    }

    return Pair(workflows, parts)
}

fun parseWorkflow(line: String): Pair<String, List<Rule>> {
    val idx = line.indexOf("{")
    val workflowName = line.substring(0, idx)
    val rules = line.substring(idx + 1, line.length - 1).split(",").map { ruleStr ->
        val colonIdx = ruleStr.indexOf(":")
        if (colonIdx == -1) {
            Rule(' ', ' ', 0, ruleStr)
        } else {
            val category = ruleStr[0]
            val operator = ruleStr[1]
            val num = ruleStr.substring(2, colonIdx).toInt()
            val workflowNameRule = ruleStr.substring(colonIdx + 1)
            Rule(category, operator, num, workflowNameRule)
        }
    }
    return Pair(workflowName, rules)
}

fun parsePart(line: String): Part {
    val values = line.substring(1, line.length - 1).split(",").map { it.split("=")[1].toInt() }
    return mapOf('x' to values[0], 'm' to values[1], 'a' to values[2], 's' to values[3])
}

fun applyWorkflow(part: Part, workflows: Workflows, workflowName: String): Boolean {
    if (workflowName == "A") return true
    if (workflowName == "R") return false

    for (rule in workflows[workflowName]!!) {
        val rating = part[rule.category] ?: 0
        val isValid = when (rule.operator) {
            '>' -> rating > rule.num
            '<' -> rating < rule.num
            else -> true
        }
        if (isValid) {
            return applyWorkflow(part, workflows, rule.workflowName)
        }
    }
    return false
}

fun applyWorkflowInterval(partInterval: PartInterval, workflows: Workflows, workflowName: String): Long {
    if (workflowName == "A") {
        var res = 1L
        for (interval in partInterval.values) {
            res *= (interval.end - interval.start + 1)
        }
        return res
    }
    if (workflowName == "R") return 0

    var res = 0L
    for (rule in workflows[workflowName]!!) {
        val ratingInterval = partInterval[rule.category] ?: Interval(0,0)
        var validRatingInterval = Interval(0,0)
        var invalidRatingInterval = Interval(0,0)

        when (rule.operator) {
            '>' -> {
                invalidRatingInterval = Interval(ratingInterval.start, rule.num)
                validRatingInterval = Interval(rule.num + 1, ratingInterval.end)
            }
            '<' -> {
                validRatingInterval = Interval(ratingInterval.start, rule.num - 1)
                invalidRatingInterval = Interval(rule.num, ratingInterval.end)
            }
            else -> validRatingInterval = ratingInterval
        }

        val newPart = partInterval.toMutableMap()
        newPart[rule.category] = validRatingInterval
        res += applyWorkflowInterval(newPart, workflows, rule.workflowName)

        partInterval[rule.category] = invalidRatingInterval
        if(invalidRatingInterval.start > invalidRatingInterval.end) break
    }
    return res
}

fun solve(input: List<String>): Long {
    val startWorkflow = "in"
    val minRating = 1
    val maxRating = 4000

    val (workflows, _) = parseInput(input)
    val partInterval = mutableMapOf(
        'x' to Interval(minRating, maxRating),
        'm' to Interval(minRating, maxRating),
        'a' to Interval(minRating, maxRating),
        's' to Interval(minRating, maxRating)
    )

    return applyWorkflowInterval(partInterval, workflows, startWorkflow)
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}

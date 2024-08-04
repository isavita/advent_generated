import java.io.File
import java.util.regex.Pattern

data class Rule(val name: String, val ranges: List<IntRange>) {
    fun isValid(value: Int) = ranges.any { value in it }
}

fun main() {
    val input = File("input.txt").readLines()
    val rules = mutableListOf<Rule>()
    val myTicket = mutableListOf<Int>()
    val nearbyTickets = mutableListOf<List<Int>>()
    val sectionRegex = Pattern.compile("^([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$")
    var section = 0

    input.forEach { line ->
        if (line.isEmpty()) {
            section++
        } else {
            when (section) {
                0 -> {
                    val matcher = sectionRegex.matcher(line)
                    if (matcher.matches()) {
                        val rule = Rule(
                            matcher.group(1),
                            listOf(
                                matcher.group(2).toInt()..matcher.group(3).toInt(),
                                matcher.group(4).toInt()..matcher.group(5).toInt()
                            )
                        )
                        rules.add(rule)
                    }
                }
                1 -> {
                    if (line != "your ticket:") {
                        myTicket.addAll(line.split(",").map { it.toInt() })
                    }
                }
                2 -> {
                    if (line != "nearby tickets:") {
                        val ticket = line.split(",").map { it.toInt() }
                        if (isValidTicket(ticket, rules)) {
                            nearbyTickets.add(ticket)
                        }
                    }
                }
            }
        }
    }

    val fieldPositions = solveFieldPositions(rules, nearbyTickets)
    val departureProduct = calculateDepartureProduct(myTicket, fieldPositions)

    println(departureProduct)
}

fun isValidTicket(ticket: List<Int>, rules: List<Rule>): Boolean {
    return ticket.all { value -> rules.any { it.isValid(value) } }
}

fun solveFieldPositions(rules: List<Rule>, tickets: List<List<Int>>): Map<String, Int> {
    val validPositions = mutableMapOf<String, MutableMap<Int, Boolean>>()
    rules.forEach { rule ->
        validPositions[rule.name] = mutableMapOf()
        (0 until tickets[0].size).forEach { i ->
            val valid = tickets.all { ticket -> rule.isValid(ticket[i]) }
            if (valid) validPositions[rule.name]!![i] = true
        }
    }

    val fieldPositions = mutableMapOf<String, Int>()
    while (fieldPositions.size < rules.size) {
        val keysCopy = validPositions.keys.toList()
        keysCopy.forEach { name ->
            val positions = validPositions[name]!!
            if (positions.size == 1) {
                val pos = positions.keys.first()
                fieldPositions[name] = pos
                validPositions.values.forEach { it.remove(pos) }
                validPositions.remove(name)
            }
        }
    }
    return fieldPositions
}

fun calculateDepartureProduct(ticket: List<Int>, fieldPositions: Map<String, Int>): Long {
    return fieldPositions.filterKeys { it.startsWith("departure") }
        .map { (_, pos) -> ticket[pos].toLong() }
        .reduce { acc, value -> acc * value }
}

import java.io.File
import java.util.regex.Pattern

fun evaluateSimple(expression: String): Long {
    val parts = "\\d+|\\+|\\*".toRegex().findAll(expression).map { it.value }.toList()
    var total = parts[0].toLong()
    var i = 1
    while (i < parts.size) {
        when (parts[i]) {
            "+" -> total += parts[i + 1].toLong()
            "*" -> total *= parts[i + 1].toLong()
        }
        i += 2
    }
    return total
}

fun evaluateAdvanced(expression: String): Long {
    val parts = "\\d+|\\+|\\*".toRegex().findAll(expression).map { it.value }.toMutableList()
    while (parts.contains("+")) {
        val i = parts.indexOf("+")
        val total = parts[i - 1].toLong() + parts[i + 1].toLong()
        parts[i - 1] = total.toString()
        parts.removeAt(i)
        parts.removeAt(i)
    }
    var total = parts[0].toLong()
    var i = 1
    while (i < parts.size) {
        total *= parts[i + 1].toLong()
        i += 2
    }
    return total
}

fun evaluateExpression(expression: String, evaluateFn: (String) -> Long): Long {
    var expr = expression
    while (expr.contains('(')) {
        val start = expr.lastIndexOf('(')
        val end = start + expr.substring(start).indexOf(')')
        expr = expr.substring(0, start) + evaluateFn(expr.substring(start + 1, end)) + expr.substring(end + 1)
    }
    return evaluateFn(expr)
}

fun main() {
    val expressions = File("input.txt").readText().trim().split("\n")

    val resultPart1 = expressions.sumOf { evaluateExpression(it, ::evaluateSimple) }
    val resultPart2 = expressions.sumOf { evaluateExpression(it, ::evaluateAdvanced) }

    println(resultPart1)
    println(resultPart2)
}

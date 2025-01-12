
import java.io.File
import java.util.*

fun main() {
    val sum = File("input.txt").readLines().sumOf { evaluate(it) }
    println(sum)
}

fun evaluate(expression: String): Long {
    val tokens = tokenize(expression)
    return evaluateTokens(tokens)
}

fun tokenize(expression: String): List<String> {
    return expression.replace("(", "( ").replace(")", " )").split("\\s+".toRegex()).filter { it.isNotBlank() }
}

fun evaluateTokens(tokens: List<String>): Long {
    val ops = Stack<String>()
    val vals = Stack<Long>()

    for (token in tokens) {
        when (token) {
            "(" -> ops.push(token)
            "+", "*" -> {
                while (ops.isNotEmpty() && ops.peek() != "(") {
                    vals.push(applyOp(ops.pop(), vals.pop(), vals.pop()))
                }
                ops.push(token)
            }
            ")" -> {
                while (ops.peek() != "(") {
                    vals.push(applyOp(ops.pop(), vals.pop(), vals.pop()))
                }
                ops.pop()
            }
            else -> vals.push(token.toLong())
        }
    }
    while (ops.isNotEmpty()) {
        vals.push(applyOp(ops.pop(), vals.pop(), vals.pop()))
    }
    return vals.pop()
}

fun applyOp(op: String, b: Long, a: Long): Long {
    return when (op) {
        "+" -> a + b
        "*" -> a * b
        else -> throw IllegalArgumentException("Unknown operator: $op")
    }
}

import java.io.File

class Monkey(
    val name: String,
    val job: String,
    val value: Long? = null,
    val left: String? = null,
    val op: Char? = null,
    val right: String? = null
)

fun main() {
    val monkeys = File("input.txt")
        .readLines()
        .map { line ->
            val parts = line.split(": ")
            val job = parts[1].split(" ")
            if (job.size == 1) {
                Monkey(parts[0], job[0], job[0].toLong())
            } else {
                Monkey(parts[0], "", null, job[0], job[1][0], job[2])
            }
        }
        .associateBy { it.name }
        .toMap()

    val result = monkeys["root"]!!.computeValue(monkeys)
    println("The final answer is $result.")
}

fun Monkey.computeValue(monkeys: Map<String, Monkey>): Long {
    return if (value != null) {
        value!!
    } else {
        val leftValue = monkeys[left]!!.computeValue(monkeys)
        val rightValue = monkeys[right]!!.computeValue(monkeys)
        when (op) {
            '+' -> leftValue + rightValue
            '-' -> leftValue - rightValue
            '*' -> leftValue * rightValue
            '/' -> leftValue / rightValue
            else -> throw IllegalStateException("Invalid operator: $op")
        }
    }
}
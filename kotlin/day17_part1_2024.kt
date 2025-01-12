
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    var A = 0
    var B = 0
    var C = 0
    var program = emptyList<Int>()

    for (line in lines) {
        val trimmedLine = line.trim()
        if (trimmedLine.isEmpty()) continue
        when {
            trimmedLine.startsWith("Register A:") -> A = trimmedLine.substringAfter(":").trim().toInt()
            trimmedLine.startsWith("Register B:") -> B = trimmedLine.substringAfter(":").trim().toInt()
            trimmedLine.startsWith("Register C:") -> C = trimmedLine.substringAfter(":").trim().toInt()
            trimmedLine.startsWith("Program:") -> program = trimmedLine.substringAfter(":").trim().split(",").map { it.trim().toInt() }
        }
    }

    fun getComboVal(op: Int): Int = when (op) {
        in 0..3 -> op
        4 -> A
        5 -> B
        6 -> C
        else -> throw IllegalArgumentException("invalid combo operand")
    }

    val outputVals = mutableListOf<String>()
    var ip = 0
    while (ip < program.size) {
        val opcode = program[ip]
        if (ip + 1 >= program.size) break
        val operand = program[ip + 1]

        when (opcode) {
            0 -> {
                val den = getComboVal(operand)
                A = if (den == 0) 0 else A shr den
                ip += 2
            }
            1 -> {
                B = B xor operand
                ip += 2
            }
            2 -> {
                B = getComboVal(operand) % 8
                ip += 2
            }
            3 -> {
                ip = if (A != 0) operand else ip + 2
            }
            4 -> {
                B = B xor C
                ip += 2
            }
            5 -> {
                outputVals.add((getComboVal(operand) % 8).toString())
                ip += 2
            }
            6 -> {
                val den = getComboVal(operand)
                B = A shr den
                ip += 2
            }
            7 -> {
                val den = getComboVal(operand)
                C = A shr den
                ip += 2
            }
            else -> break
        }
    }
    println(outputVals.joinToString(","))
}

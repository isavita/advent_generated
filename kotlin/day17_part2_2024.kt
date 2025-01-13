
import java.io.File

data class Program(var a: Long, var b: Long, var c: Long, val program: List<Int>)

data class Pair(val a: Int, val b: Long)

fun computeOperand(val_: Int, a: Long, b: Long, c: Long): Long {
    return when (val_) {
        0, 1, 2, 3 -> val_.toLong()
        4 -> a
        5 -> b
        6 -> c
        else -> throw IllegalArgumentException("Invalid combo operand: $val_")
    }
}

fun simulateComputer(program: Program): List<Int> {
    val outs = mutableListOf<Int>()
    var a = program.a
    var b = program.b
    var c = program.c
    val input = program.program

    var i = 1
    while (i <= input.size) {
        val cmd = input[i - 1]
        when (cmd) {
            0 -> a = a shr computeOperand(input[i], a, b, c).toInt()
            1 -> b = b xor input[i].toLong()
            2 -> b = computeOperand(input[i], a, b, c) % 8
            3 -> if (a != 0L) i = input[i] - 1
            4 -> b = b xor c
            5 -> outs.add((computeOperand(input[i], a, b, c) % 8).toInt())
            6 -> b = a shr computeOperand(input[i], a, b, c).toInt()
            7 -> c = a shr computeOperand(input[i], a, b, c).toInt()
            else -> throw IllegalArgumentException("Invalid opcode: $cmd")
        }
        i += 2
    }
    return outs
}

fun check(p: Program): List<Long> {
    val program = p.program
    val valids = mutableListOf<Long>()
    val stack = mutableListOf(Pair(0, 0))
    val seen = mutableMapOf<Pair, Boolean>()

    while (stack.isNotEmpty()) {
        val state = stack.removeLast()

        if (seen[state] == true) continue
        seen[state] = true

        val depth = state.a
        val score = state.b

        if (depth == program.size) {
            valids.add(score)
        } else {
            for (i in 0 until 8) {
                val newScore = i + 8 * score
                val testProgram = Program(newScore, p.b, p.c, program)
                val result = simulateComputer(testProgram)
                if (result.isNotEmpty() && result[0] == program[program.size - 1 - depth]) {
                    stack.add(Pair(depth + 1, newScore))
                }
            }
        }
    }
    return valids
}

fun main() {
    val lines = File("input.txt").readLines()
    var a = 0L
    var b = 0L
    var c = 0L
    val program = mutableListOf<Int>()

    for (line in lines) {
        val trimmedLine = line.trim()
        when {
            trimmedLine.startsWith("Register A:") -> {
                a = trimmedLine.split(":")[1].trim().toLong()
            }
            trimmedLine.startsWith("Register B:") -> {
                b = trimmedLine.split(":")[1].trim().toLong()
            }
            trimmedLine.startsWith("Register C:") -> {
                c = trimmedLine.split(":")[1].trim().toLong()
            }
            trimmedLine.startsWith("Program:") -> {
                program.addAll(trimmedLine.split(":")[1].trim().split(",").map { it.trim().toInt() })
            }
        }
    }

    val p = Program(a, b, c, program)
    val validValues = check(p)
    println(validValues.minOrNull())
}

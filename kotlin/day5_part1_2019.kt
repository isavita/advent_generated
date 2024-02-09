import java.io.File

fun getMode(instruction: Int, position: Int): Int {
    return instruction / Math.pow(10.0, (position + 1).toDouble()).toInt() % 10
}

fun getParam(program: MutableList<Int>, pointer: Int, mode: Int): Int {
    if (mode == 0) {
        return program[program[pointer]]
    }
    return program[pointer]
}

fun runProgram(program: MutableList<Int>, input: Int): Int {
    var output = 0
    var pointer = 0
    while (pointer < program.size) {
        val instruction = program[pointer]
        val opcode = instruction % 100

        when (opcode) {
            1, 2 -> {
                val param1 = getParam(program, pointer + 1, getMode(instruction, 1))
                val param2 = getParam(program, pointer + 2, getMode(instruction, 2))
                val result = if (opcode == 1) param1 + param2 else param1 * param2
                program[program[pointer + 3]] = result
                pointer += 4
            }
            3 -> {
                program[program[pointer + 1]] = input
                pointer += 2
            }
            4 -> {
                output = getParam(program, pointer + 1, getMode(instruction, 1))
                pointer += 2
            }
            99 -> return output
            else -> throw RuntimeException("Unknown opcode: $opcode")
        }
    }
    return output
}

fun main(args: Array<String>) {
    val data = File("input.txt").readText().trim().split(",")
    val program = data.map { it.toInt() }.toMutableList()
    println(runProgram(program, 1))
}
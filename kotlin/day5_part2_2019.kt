import java.io.File

fun main(args: Array<String>) {
    val program = File("input.txt").readText().split(",").map { it.toInt() }.toMutableList()

    var input = 5
    var output = 0
    var i = 0
    while (i < program.size) {
        val opcode = program[i] % 100
        var modes = program[i] / 100
        val param1Mode = modes % 10
        modes /= 10
        val param2Mode = modes % 10

        when (opcode) {
            1 -> {
                val p1 = getValue(program, i + 1, param1Mode)
                val p2 = getValue(program, i + 2, param2Mode)
                val p3 = program[i + 3]
                program[p3] = p1 + p2
                i += 4
            }
            2 -> {
                val p1 = getValue(program, i + 1, param1Mode)
                val p2 = getValue(program, i + 2, param2Mode)
                val p3 = program[i + 3]
                program[p3] = p1 * p2
                i += 4
            }
            3 -> {
                program[program[i + 1]] = input
                i += 2
            }
            4 -> {
                output = getValue(program, i + 1, param1Mode)
                println(output)
                i += 2
            }
            5 -> {
                val p1 = getValue(program, i + 1, param1Mode)
                val p2 = getValue(program, i + 2, param2Mode)
                if (p1 != 0) {
                    i = p2
                } else {
                    i += 3
                }
            }
            6 -> {
                val p1 = getValue(program, i + 1, param1Mode)
                val p2 = getValue(program, i + 2, param2Mode)
                if (p1 == 0) {
                    i = p2
                } else {
                    i += 3
                }
            }
            7 -> {
                val p1 = getValue(program, i + 1, param1Mode)
                val p2 = getValue(program, i + 2, param2Mode)
                val p3 = program[i + 3]
                program[p3] = if (p1 < p2) 1 else 0
                i += 4
            }
            8 -> {
                val p1 = getValue(program, i + 1, param1Mode)
                val p2 = getValue(program, i + 2, param2Mode)
                val p3 = program[i + 3]
                program[p3] = if (p1 == p2) 1 else 0
                i += 4
            }
            99 -> return
            else -> throw IllegalArgumentException("Invalid opcode")
        }
    }
}

fun getValue(program: List<Int>, pos: Int, mode: Int): Int {
    return if (mode == 0) {
        program[program[pos]]
    } else {
        program[pos]
    }
}
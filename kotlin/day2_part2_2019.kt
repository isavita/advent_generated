fun main() {
    val input = "input.txt".readFile()
    val program = input.split(",").map { it.toInt() }.toMutableList()

    program[1] = 12
    program[2] = 2
    runProgram(program)
    println(program[0])

    val targetOutput = 19690720
    for (noun in 0..99) {
        for (verb in 0..99) {
            val memory = input.split(",").map { it.toInt() }.toMutableList()
            memory[1] = noun
            memory[2] = verb
            runProgram(memory)
            if (memory[0] == targetOutput) {
                println(100 * noun + verb)
                return
            }
        }
    }
}

fun runProgram(program: MutableList<Int>) {
    var ip = 0
    while (true) {
        when (program[ip]) {
            1 -> {
                val a = program[program[ip + 1]]
                val b = program[program[ip + 2]]
                program[program[ip + 3]] = a + b
            }
            2 -> {
                val a = program[program[ip + 1]]
                val b = program[program[ip + 2]]
                program[program[ip + 3]] = a * b
            }
            99 -> return
        }
        ip += 4
    }
}

fun String.readFile(): String {
    return java.io.File(this).readText().trim()
}
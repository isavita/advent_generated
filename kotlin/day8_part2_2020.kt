import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val instructions = file.readLines()

    for (i in instructions.indices) {
        val (op, arg) = parseInstruction(instructions[i])
        if (op == "acc") {
            continue
        }

        val modifiedInstructions = instructions.toMutableList()
        if (op == "jmp") {
            modifiedInstructions[i] = "nop $arg"
        } else {
            modifiedInstructions[i] = "jmp $arg"
        }

        val result = executeBootCode(modifiedInstructions)
        if (result.second) {
            println(result.first)
            break
        }
    }
}

fun executeBootCode(instructions: List<String>): Pair<Int, Boolean> {
    var accumulator = 0
    val visited = mutableSetOf<Int>()
    var currentInstruction = 0

    while (currentInstruction < instructions.size) {
        if (visited.contains(currentInstruction)) {
            return Pair(accumulator, false)
        }

        visited.add(currentInstruction)
        val (op, arg) = parseInstruction(instructions[currentInstruction])

        when (op) {
            "acc" -> {
                accumulator += arg
                currentInstruction++
            }
            "jmp" -> currentInstruction += arg
            "nop" -> currentInstruction++
        }
    }

    return Pair(accumulator, true)
}

fun parseInstruction(instruction: String): Pair<String, Int> {
    val parts = instruction.split(" ")
    val op = parts[0]
    val arg = parts[1].toInt()
    return Pair(op, arg)
}
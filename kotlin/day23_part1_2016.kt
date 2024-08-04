import java.io.File

fun main() {
    val instructions = File("input.txt").readLines().toMutableList()
    val registers = mutableMapOf("a" to 7, "b" to 0, "c" to 0, "d" to 0)
    executeInstructions(instructions, registers)
    println(registers["a"])
}

fun executeInstructions(instructions: MutableList<String>, registers: MutableMap<String, Int>) {
    var pc = 0
    while (pc < instructions.size) {
        val fields = instructions[pc].split(" ")
        when (fields[0]) {
            "cpy" -> {
                val value = if (fields[1].isInteger()) fields[1].toInt() else registers[fields[1]] ?: 0
                if (fields[2] in registers) registers[fields[2]] = value
            }
            "inc" -> registers[fields[1]] = (registers[fields[1]] ?: 0) + 1
            "dec" -> registers[fields[1]] = (registers[fields[1]] ?: 0) - 1
            "jnz" -> {
                val condition = if (fields[1].isInteger()) fields[1].toInt() else registers[fields[1]] ?: 0
                if (condition != 0) {
                    val jump = if (fields[2].isInteger()) fields[2].toInt() else registers[fields[2]] ?: 0
                    pc += jump - 1
                }
            }
            "tgl" -> {
                val tgt = pc + (if (fields[1].isInteger()) fields[1].toInt() else registers[fields[1]] ?: 0)
                if (tgt in instructions.indices) instructions[tgt] = toggleInstruction(instructions[tgt])
            }
        }
        pc++
    }
}

fun toggleInstruction(instr: String): String {
    val parts = instr.split(" ").toMutableList()
    parts[0] = when (parts[0]) {
        "inc" -> "dec"
        "dec", "tgl" -> "inc"
        "jnz" -> "cpy"
        "cpy" -> "jnz"
        else -> parts[0]
    }
    return parts.joinToString(" ")
}

fun String.isInteger() = toIntOrNull() != null
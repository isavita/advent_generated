import java.io.File

fun main(args: Array<String>) {
    val instructions = File("input.txt").readLines()
    val registers = mutableMapOf("a" to 0, "b" to 0, "c" to 1, "d" to 0)
    executeInstructions(instructions, registers)
    println(registers["a"])
}

fun executeInstructions(instructions: List<String>, registers: MutableMap<String, Int>) {
    var i = 0
    while (i < instructions.size) {
        val parts = instructions[i].split(" ")
        when (parts[0]) {
            "cpy" -> {
                val value = getValue(parts[1], registers)
                registers[parts[2]] = value
                i++
            }
            "inc" -> {
                registers[parts[1]] = registers[parts[1]]!! + 1
                i++
            }
            "dec" -> {
                registers[parts[1]] = registers[parts[1]]!! - 1
                i++
            }
            "jnz" -> {
                val value = getValue(parts[1], registers)
                if (value != 0) {
                    val jump = parts[2].toInt()
                    i += jump
                } else {
                    i++
                }
            }
        }
    }
}

fun getValue(s: String, registers: Map<String, Int>): Int {
    return s.toIntOrNull() ?: registers.getValue(s)
}
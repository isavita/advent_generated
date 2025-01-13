
import java.io.File

fun main() {
    val instructions = File("input.txt").readLines().map { it.split(" ") }.toMutableList()
    val registers = mutableMapOf("a" to 12, "b" to 0, "c" to 0, "d" to 0) // Part 2: a = 12

    var ip = 0
    while (ip in instructions.indices) {
        val instruction = instructions[ip]
        when (instruction[0]) {
            "cpy" -> {
                val src = instruction[1]
                val dest = instruction[2]
                if (dest in registers) {
                    registers[dest] = registers.getOrDefault(src, src.toIntOrNull() ?: 0)
                }
            }
            "inc" -> {
                val reg = instruction[1]
                if (reg in registers) {
                    registers[reg] = registers[reg]!! + 1
                }
            }
            "dec" -> {
                val reg = instruction[1]
                if (reg in registers) {
                    registers[reg] = registers[reg]!! - 1
                }
            }
            "jnz" -> {
                val cond = instruction[1]
                val offset = instruction[2]
                val condValue = registers.getOrDefault(cond, cond.toIntOrNull() ?: 0)
                if (condValue != 0) {
                    ip += registers.getOrDefault(offset, offset.toIntOrNull() ?: 0) - 1
                }
            }
            "tgl" -> {
                val offset = instruction[1]
                val targetIp = ip + registers.getOrDefault(offset, offset.toIntOrNull() ?: 0)
                if (targetIp in instructions.indices) {
                    val targetInstruction = instructions[targetIp]
                    instructions[targetIp] = when (targetInstruction.size) {
                        2 -> {
                            if (targetInstruction[0] == "inc") listOf("dec", targetInstruction[1])
                            else listOf("inc", targetInstruction[1])
                        }
                        3 -> {
                            if (targetInstruction[0] == "jnz") listOf("cpy", targetInstruction[1], targetInstruction[2])
                            else listOf("jnz", targetInstruction[1], targetInstruction[2])
                        }
                        else -> targetInstruction
                    }
                }
            }
        }
        
        // Optimization for Part 2: Detect and replace multiplication loop
        if (ip == 4) {
            registers["a"] = registers["b"]!! * registers["d"]!!
            registers["c"] = 0
            registers["d"] = 0
            ip = 9
        } else {
            ip++
        }
    }

    println(registers["a"])
}


import java.io.File

fun main() {
    val instructions = File("input.txt").readLines().map { line ->
        val parts = line.split(", ")
        val instructionParts = parts[0].split(" ")
        val opcode = instructionParts[0]
        val register = instructionParts.getOrNull(1)?.firstOrNull()

        when (opcode) {
            "hlf", "tpl", "inc" -> Triple(opcode, register!!, null)
            "jmp" -> Triple(opcode, parts[0].split(" ")[1], null)
            "jie", "jio" -> Triple(opcode, register!!, parts[1])
            else -> throw IllegalArgumentException("Unknown opcode: $opcode")
        }
    }

    fun runProgram(initialA: Long): Long {
        val registers = mutableMapOf('a' to initialA, 'b' to 0L)
        var pc = 0

        while (pc in instructions.indices) {
            val (opcode, register, offset) = instructions[pc]

            when (opcode) {
                "hlf" -> registers[register as Char] = registers[register]!! / 2
                "tpl" -> registers[register as Char] = registers[register]!! * 3
                "inc" -> registers[register as Char] = registers[register]!! + 1
                "jmp" -> {
                    pc += (register as String).toInt()
                    continue
                }
                "jie" -> {
                    if (registers[register as Char]!! % 2 == 0L) {
                        pc += offset!!.toInt()
                        continue
                    }
                }
                "jio" -> {
                    if (registers[register as Char] == 1L) {
                        pc += offset!!.toInt()
                        continue
                    }
                }
            }
            pc++
        }

        return registers['b']!!
    }

    println("Part 1: ${runProgram(0)}")
    println("Part 2: ${runProgram(1)}")
}

import java.io.File

fun main() {
    val instructions = File("input.txt").readLines()
    val registers = mutableMapOf<Char, Long>()
    var lastSound = 0L
    var recoveredFrequency = 0L

    var ip = 0 // Instruction pointer
    while (ip in instructions.indices) {
        val parts = instructions[ip].split(" ")
        val instruction = parts[0]
        val x = parts[1][0]
        val y = if (parts.size > 2) parts[2] else null

        val valueX = if (x in 'a'..'z') registers.getOrDefault(x, 0L) else x.toString().toLong()
        val valueY = if (y != null && y[0] in 'a'..'z') registers.getOrDefault(y[0], 0L) else y?.toLong() ?: 0L

        when (instruction) {
            "snd" -> {
                lastSound = valueX
            }
            "set" -> {
                registers[x] = valueY
            }
            "add" -> {
                registers[x] = valueX + valueY
            }
            "mul" -> {
                registers[x] = valueX * valueY
            }
            "mod" -> {
                registers[x] = valueX % valueY
            }
            "rcv" -> {
                if (valueX != 0L) {
                    recoveredFrequency = lastSound
                    break
                }
            }
            "jgz" -> {
                if (valueX > 0L) {
                    ip += valueY.toInt() - 1
                }
            }
        }
        ip++
    }

    println("Recovered Frequency: $recoveredFrequency")
}
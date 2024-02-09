import java.io.File

fun main(args: Array<String>) {
    val instructions = File("input.txt").readLines()

    for (a in 1..Int.MAX_VALUE) {
        if (producesClockSignal(a, instructions)) {
            println(a)
            break
        }
    }
}

fun producesClockSignal(a: Int, instructions: List<String>): Boolean {
    val registers = mutableMapOf("a" to a, "b" to 0, "c" to 0, "d" to 0)
    var lastOutput = 0
    var outputCount = 0

    var i = 0
    while (i < instructions.size) {
        val parts = instructions[i].split(" ")
        when (parts[0]) {
            "cpy" -> registers[parts[2]] = parts[1].toIntOrNull() ?: registers[parts[1]]!!
            "inc" -> registers[parts[1]] = registers[parts[1]]!! + 1
            "dec" -> registers[parts[1]] = registers[parts[1]]!! - 1
            "jnz" -> {
                val value = parts[1].toIntOrNull() ?: registers[parts[1]]!!
                if (value != 0) {
                    i += parts[2].toInt()
                    continue
                }
            }
            "out" -> {
                val value = parts[1].toIntOrNull() ?: registers[parts[1]]!!
                if (value != 0 && value != 1) {
                    return false
                }
                if (outputCount > 0 && value == lastOutput) {
                    return false
                }
                lastOutput = value
                outputCount++
                if (outputCount > 50) {
                    return true
                }
            }
        }
        i++
    }
    return false
}
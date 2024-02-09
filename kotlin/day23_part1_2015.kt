import java.io.File

fun main(args: Array<String>) {
    val instructions = File("input.txt").readLines()

    val registers = mutableMapOf("a" to 0, "b" to 0)

    var i = 0
    while (i < instructions.size) {
        val parts = instructions[i].split(" ")

        when (parts[0]) {
            "hlf" -> registers[parts[1]] = registers[parts[1]]!! / 2
            "tpl" -> registers[parts[1]] = registers[parts[1]]!! * 3
            "inc" -> registers[parts[1]] = registers[parts[1]]!! + 1
            "jmp" -> {
                val offset = parts[1].toInt()
                i += offset - 1
            }
            "jie" -> {
                if (registers[parts[1].substring(0, 1)]!! % 2 == 0) {
                    val offset = parts[2].toInt()
                    i += offset - 1
                }
            }
            "jio" -> {
                if (registers[parts[1].substring(0, 1)]!! == 1) {
                    val offset = parts[2].toInt()
                    i += offset - 1
                }
            }
            else -> error("Unknown instruction: ${parts[0]}")
        }

        i++
    }

    println(registers["b"])
}
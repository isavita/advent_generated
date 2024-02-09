import java.io.File

fun main(args: Array<String>) {
    val instructions = File("input.txt").readLines()
    var mulCount = 0
    var pointer = 0
    val registers = mutableMapOf<String, Int>()

    fun getValue(s: String): Int {
        return if (s.toIntOrNull() != null) {
            s.toInt()
        } else {
            registers.getOrDefault(s, 0)
        }
    }

    while (pointer in instructions.indices) {
        val parts = instructions[pointer].split(" ")
        val cmd = parts[0]
        val x = parts[1]
        val y = parts[2]

        when (cmd) {
            "set" -> registers[x] = getValue(y)
            "sub" -> registers[x] = registers.getOrDefault(x, 0) - getValue(y)
            "mul" -> {
                registers[x] = registers.getOrDefault(x, 0) * getValue(y)
                mulCount++
            }
            "jnz" -> {
                if (getValue(x) != 0) {
                    pointer += getValue(y) - 1
                }
            }
        }
        pointer++
    }

    println(mulCount)
}
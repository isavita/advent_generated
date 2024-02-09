import java.io.File

fun main() {
    val wires = mutableMapOf<String, Int>()
    val instructions = File("input.txt").readLines()

    fun getValue(input: String): Int {
        return if (input.toIntOrNull() != null) {
            input.toInt()
        } else {
            wires.getOrPut(input) {
                val parts = instructions.find { it.endsWith(" -> $input") }!!.split(" -> ")
                val value = when {
                    parts[0].contains("AND") -> {
                        val (x, y) = parts[0].split(" AND ")
                        getValue(x) and getValue(y)
                    }
                    parts[0].contains("OR") -> {
                        val (x, y) = parts[0].split(" OR ")
                        getValue(x) or getValue(y)
                    }
                    parts[0].contains("LSHIFT") -> {
                        val (x, shift) = parts[0].split(" LSHIFT ")
                        getValue(x) shl shift.toInt()
                    }
                    parts[0].contains("RSHIFT") -> {
                        val (x, shift) = parts[0].split(" RSHIFT ")
                        getValue(x) shr shift.toInt()
                    }
                    parts[0].contains("NOT") -> {
                        val x = parts[0].substringAfter("NOT ")
                        getValue(x).inv() and 0xFFFF
                    }
                    else -> getValue(parts[0])
                }
                wires[input] = value
                value
            }
        }
    }

    val result = getValue("a")
    println(result)
}

import java.io.File

class VM(filename: String) {
    private val code = mutableMapOf<Int, Long>()
    private var ip = 0
    private var relativeBase = 0
    var input = mutableListOf<Long>()
    var output = mutableListOf<Long>()

    init {
        File(filename).readText().trim().split(',').forEachIndexed { i, s -> code[i] = s.toLong() }
    }

    fun run() {
        while (true) {
            val cmd = code.getOrDefault(ip, 0).toInt()
            val opcode = cmd % 100
            val modes = listOf((cmd / 100) % 10, (cmd / 1000) % 10, (cmd / 10000) % 10)

            fun getParam(index: Int): Long {
                val mode = modes[index - 1]
                val value = code.getOrDefault(ip + index, 0)
                return when (mode) {
                    0 -> code.getOrDefault(value.toInt(), 0)
                    1 -> value
                    2 -> code.getOrDefault(relativeBase + value.toInt(), 0)
                    else -> error("Invalid mode")
                }
            }

            fun getAddress(index: Int): Int {
                val mode = modes[index - 1]
                val value = code.getOrDefault(ip + index, 0).toInt()
                return when (mode) {
                    0 -> value
                    2 -> relativeBase + value
                    else -> error("Invalid mode")
                }
            }

            when (opcode) {
                1 -> {
                    code[getAddress(3)] = getParam(1) + getParam(2)
                    ip += 4
                }
                2 -> {
                    code[getAddress(3)] = getParam(1) * getParam(2)
                    ip += 4
                }
                3 -> {
                    code[getAddress(1)] = input.removeAt(0)
                    ip += 2
                }
                4 -> {
                    output.add(getParam(1))
                    ip += 2
                }
                5 -> ip = if (getParam(1) != 0L) getParam(2).toInt() else ip + 3
                6 -> ip = if (getParam(1) == 0L) getParam(2).toInt() else ip + 3
                7 -> {
                    code[getAddress(3)] = if (getParam(1) < getParam(2)) 1 else 0
                    ip += 4
                }
                8 -> {
                    code[getAddress(3)] = if (getParam(1) == getParam(2)) 1 else 0
                    ip += 4
                }
                9 -> {
                    relativeBase += getParam(1).toInt()
                    ip += 2
                }
                99 -> return
                else -> error("Unknown opcode $opcode")
            }
        }
    }
}

fun beam(x: Int, y: Int, filename: String): Boolean {
    val vm = VM(filename)
    vm.input = mutableListOf(x.toLong(), y.toLong())
    vm.run()
    return vm.output[0] == 1L
}

fun main() {
    var sum = 0
    for (y in 0 until 50) {
        for (x in 0 until 50) {
            if (beam(x, y, "input.txt")) {
                sum++
            }
        }
    }
    println(sum)
}


import java.io.File

fun main() {
    val memory = File("input.txt").readText().trim().split(",").mapIndexed { index, s -> index to s.toLong() }.toMap().toMutableMap()
    println(runIntcode(memory, 2))
}

fun runIntcode(memory: MutableMap<Int, Long>, input: Long): Long {
    var output = 0L
    var ip = 0
    var relativeBase = 0

    while (true) {
        val opcode = memory[ip]?.toInt()?.rem(100) ?: 0
        val modes = (memory[ip]?.toInt() ?: 0) / 100

        fun getParam(offset: Int): Long {
            val mode = (modes / (10.0.pow(offset - 1).toInt())) % 10
            val param = memory[ip + offset] ?: 0
            return when (mode) {
                0 -> memory[param.toInt()] ?: 0
                1 -> param
                2 -> memory[relativeBase + param.toInt()] ?: 0
                else -> throw IllegalArgumentException("unknown parameter mode")
            }
        }

        fun setParam(offset: Int, value: Long) {
            val mode = (modes / (10.0.pow(offset - 1).toInt())) % 10
            val param = memory[ip + offset] ?: 0
            when (mode) {
                0 -> memory[param.toInt()] = value
                2 -> memory[relativeBase + param.toInt()] = value
                else -> throw IllegalArgumentException("unknown parameter mode")
            }
        }

        when (opcode) {
            1 -> {
                setParam(3, getParam(1) + getParam(2))
                ip += 4
            }
            2 -> {
                setParam(3, getParam(1) * getParam(2))
                ip += 4
            }
            3 -> {
                setParam(1, input)
                ip += 2
            }
            4 -> {
                output = getParam(1)
                ip += 2
            }
            5 -> {
                if (getParam(1) != 0L) {
                    ip = getParam(2).toInt()
                } else {
                    ip += 3
                }
            }
            6 -> {
                if (getParam(1) == 0L) {
                    ip = getParam(2).toInt()
                } else {
                    ip += 3
                }
            }
            7 -> {
                if (getParam(1) < getParam(2)) {
                    setParam(3, 1)
                } else {
                    setParam(3, 0)
                }
                ip += 4
            }
            8 -> {
                if (getParam(1) == getParam(2)) {
                    setParam(3, 1)
                } else {
                    setParam(3, 0)
                }
                ip += 4
            }
            9 -> {
                relativeBase += getParam(1).toInt()
                ip += 2
            }
            99 -> return output
            else -> throw IllegalArgumentException("unknown opcode: $opcode")
        }
    }
}

private fun Double.pow(exponent: Int): Double {
    var result = 1.0
    for (i in 0 until exponent) {
        result *= this
    }
    return result
}

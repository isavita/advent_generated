
import java.io.File
import java.awt.Point

enum class Mode {
    POSITION, IMMEDIATE, RELATIVE
}

enum class Opcode(val code: Int) {
    ADD(1), MUL(2), INPUT(3), OUTPUT(4), JT(5), JF(6), LT(7), EQ(8), RBO(9), HALT(99);

    companion object {
        fun fromCode(code: Int): Opcode = values().first { it.code == code }
    }
}

fun decode(n: Int): Pair<Opcode, List<Mode>> {
    val op = Opcode.fromCode(n % 100)
    val modes = mutableListOf<Mode>()
    var temp = n / 100
    for (i in 0 until 3) {
        modes.add(when (temp % 10) {
            0 -> Mode.POSITION
            1 -> Mode.IMMEDIATE
            2 -> Mode.RELATIVE
            else -> throw IllegalArgumentException("Unknown mode")
        })
        temp /= 10
    }
    return Pair(op, modes)
}

class Machine(program: List<Int>, private val input: List<Int> = emptyList()) {
    private val data = program.withIndex().associate { it.index to it.value.toLong() }.toMutableMap()
    private var ip = 0
    private var relbase = 0
    private var inputIndex = 0
    val output = mutableListOf<Int>()

    private fun get(i: Int, mode: Mode): Long {
        return when (mode) {
            Mode.IMMEDIATE -> data.getOrDefault(i, 0)
            Mode.POSITION -> data.getOrDefault(data.getOrDefault(i, 0).toInt(), 0)
            Mode.RELATIVE -> data.getOrDefault((relbase + data.getOrDefault(i, 0)).toInt(), 0)
        }
    }

    private fun set(i: Int, mode: Mode, value: Long) {
        when (mode) {
            Mode.POSITION -> data[data.getOrDefault(i, 0).toInt()] = value
            Mode.RELATIVE -> data[(relbase + data.getOrDefault(i, 0)).toInt()] = value
            else -> throw IllegalArgumentException("Unknown mode for set")
        }
    }

    fun step(): Boolean {
        val (op, modes) = decode(data.getOrDefault(ip, 0).toInt())
        when (op) {
            Opcode.ADD -> {
                set(ip + 3, modes[2], get(ip + 1, modes[0]) + get(ip + 2, modes[1]))
                ip += 4
            }
            Opcode.MUL -> {
                set(ip + 3, modes[2], get(ip + 1, modes[0]) * get(ip + 2, modes[1]))
                ip += 4
            }
            Opcode.INPUT -> {
                set(ip + 1, modes[0], input[inputIndex++].toLong())
                ip += 2
            }
            Opcode.OUTPUT -> {
                output.add(get(ip + 1, modes[0]).toInt())
                ip += 2
            }
            Opcode.JT -> {
                if (get(ip + 1, modes[0]) != 0L) {
                    ip = get(ip + 2, modes[1]).toInt()
                } else {
                    ip += 3
                }
            }
            Opcode.JF -> {
                if (get(ip + 1, modes[0]) == 0L) {
                    ip = get(ip + 2, modes[1]).toInt()
                } else {
                    ip += 3
                }
            }
            Opcode.LT -> {
                set(ip + 3, modes[2], if (get(ip + 1, modes[0]) < get(ip + 2, modes[1])) 1 else 0)
                ip += 4
            }
            Opcode.EQ -> {
                set(ip + 3, modes[2], if (get(ip + 1, modes[0]) == get(ip + 2, modes[1])) 1 else 0)
                ip += 4
            }
            Opcode.RBO -> {
                relbase += get(ip + 1, modes[0]).toInt()
                ip += 2
            }
            Opcode.HALT -> return false
        }
        return true
    }

    fun run() {
        while (step()) {}
    }
}

fun main() {
    val program = File("input.txt").readText().trim().split(",").map { it.toInt() }
    val machine = Machine(program)
    machine.run()
    val grid = mutableMapOf<Point, Int>()
    for (i in machine.output.indices step 3) {
        grid[Point(machine.output[i], machine.output[i + 1])] = machine.output[i + 2]
    }
    println(grid.values.count { it == 2 })
}

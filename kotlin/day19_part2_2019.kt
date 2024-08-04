import java.io.File
import java.util.concurrent.LinkedBlockingQueue

class VM(filename: String) {
    private val code = mutableMapOf<Int, Int>()
    private var ip = 0
    private val input = LinkedBlockingQueue<Int>()
    private val output = LinkedBlockingQueue<Int>()
    private var relativeBase = 0

    init {
        load(filename)
    }

    private fun load(filename: String) {
        val listStr = File(filename).readText().trim().split(",")
        for ((i, s) in listStr.withIndex()) {
            code[i] = s.toInt()
        }
    }

    fun run() {
        while (true) {
            val cmd = Cmd(code[ip] ?: 0)
            val arity = when (cmd.opCode) {
                1, 2, 7, 8 -> 3
                3, 4, 9 -> 1
                5, 6 -> 2
                99 -> return
                else -> throw IllegalArgumentException("Unknown opcode ${cmd.opCode}")
            }
            val params = getParamsAddresses(ip, cmd, arity)

            when (cmd.opCode) {
                1 -> code[params[2]] = code.getOrDefault(params[0], 0) + code.getOrDefault(params[1], 0)
                2 -> code[params[2]] = code.getOrDefault(params[0], 0) * code.getOrDefault(params[1], 0)
                3 -> code[params[0]] = input.take()
                4 -> output.put(code.getOrDefault(params[0], 0))
                5 -> if (code.getOrDefault(params[0], 0) != 0) { ip = code.getOrDefault(params[1], 0); continue }
                6 -> if (code.getOrDefault(params[0], 0) == 0) { ip = code.getOrDefault(params[1], 0); continue }
                7 -> code[params[2]] = if (code.getOrDefault(params[0], 0) < code.getOrDefault(params[1], 0)) 1 else 0
                8 -> code[params[2]] = if (code.getOrDefault(params[0], 0) == code.getOrDefault(params[1], 0)) 1 else 0
                9 -> relativeBase += code.getOrDefault(params[0], 0)
            }
            ip += arity + 1
        }
    }

    private fun getParamsAddresses(pos: Int, cmd: Cmd, arity: Int): IntArray {
        val modes = cmd.modes(arity)
        val results = IntArray(arity)
        for (i in 0 until arity) {
            results[i] = getParamAddress(pos + i + 1, modes[i])
        }
        return results
    }

    private fun getParamAddress(pos: Int, mode: Int): Int {
        return when (mode) {
            0 -> code[pos] ?: 0
            1 -> pos
            2 -> relativeBase + (code[pos] ?: 0)
            else -> throw IllegalArgumentException("Unknown mode $mode")
        }
    }

    fun sendInput(value: Int) {
        input.put(value)
    }

    fun receiveOutput(): Int {
        return output.take()
    }
}

class Cmd(val value: Int) {
    val opCode: Int get() = value % 100

    fun modes(arity: Int): IntArray {
        val modeSection = value / 100
        val modes = IntArray(arity)
        for (i in 0 until arity) {
            modes[i] = (modeSection / Math.pow(10.0, i.toDouble())).toInt() % 10
        }
        return modes
    }
}

fun main() {
    var y = 20
    var x = 0

    while (true) {
        if (!beam(x, y)) {
            x++
            continue
        }

        if (!beam(x + 99, y)) {
            y++
            continue
        }

        if (!beam(x, y + 99)) {
            x++
            continue
        }

        println(x * 10000 + y)
        return
    }
}

fun beam(x: Int, y: Int): Boolean {
    val vm = VM("input.txt")
    Thread { vm.run() }.start()

    vm.sendInput(x)
    vm.sendInput(y)

    return vm.receiveOutput() == 1
}
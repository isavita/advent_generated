
import java.io.File
import java.lang.IllegalArgumentException
import kotlin.math.pow

class VM(filename: String) {
    private var code: MutableMap<Int, Long> = mutableMapOf()
    private var ip: Int = 0
    private var relativeBase: Int = 0
    private val input: ArrayDeque<Long> = ArrayDeque()
    private val output: ArrayDeque<Long> = ArrayDeque()

    init {
        load(filename)
    }

    private fun load(filename: String) {
        val file = File(filename)
        val text = file.readText().trim()
        text.split(",").forEachIndexed { index, s ->
            code[index] = s.toLong()
        }
        ip = 0
        relativeBase = 0
    }

    fun run() {
        while (true) {
            val cmd = Cmd(code[ip] ?: 0)
            when (cmd.opCode()) {
                1 -> { // add
                    val params = getParamsAddresses(ip, cmd, 3)
                    code[params[2]] = (code[params[0]] ?: 0) + (code[params[1]] ?: 0)
                    ip += 4
                }
                2 -> { // multiply
                    val params = getParamsAddresses(ip, cmd, 3)
                    code[params[2]] = (code[params[0]] ?: 0) * (code[params[1]] ?: 0)
                    ip += 4
                }
                3 -> { // read
                    val params = getParamsAddresses(ip, cmd, 1)
                    code[params[0]] = input.removeFirst()
                    ip += 2
                }
                4 -> { // write
                    val params = getParamsAddresses(ip, cmd, 1)
                    output.addLast(code[params[0]] ?: 0)
                    ip += 2
                }
                5 -> { // jump not zero
                    val params = getParamsAddresses(ip, cmd, 2)
                    if ((code[params[0]] ?: 0) != 0L) {
                        ip = (code[params[1]] ?: 0).toInt()
                    } else {
                        ip += 3
                    }
                }
                6 -> { // jump zero
                    val params = getParamsAddresses(ip, cmd, 2)
                    if ((code[params[0]] ?: 0) == 0L) {
                        ip = (code[params[1]] ?: 0).toInt()
                    } else {
                        ip += 3
                    }
                }
                7 -> { // less than
                    val params = getParamsAddresses(ip, cmd, 3)
                    code[params[2]] = if ((code[params[0]] ?: 0) < (code[params[1]] ?: 0)) 1 else 0
                    ip += 4
                }
                8 -> { // equal
                    val params = getParamsAddresses(ip, cmd, 3)
                    code[params[2]] = if ((code[params[0]] ?: 0) == (code[params[1]] ?: 0)) 1 else 0
                    ip += 4
                }
                9 -> { // change relative base
                    val params = getParamsAddresses(ip, cmd, 1)
                    relativeBase += (code[params[0]] ?: 0).toInt()
                    ip += 2
                }
                99 -> return // halt
                else -> throw IllegalArgumentException("Invalid opcode: ${cmd.opCode()}")
            }
        }
    }

    private fun getParamsAddresses(pos: Int, cmd: Cmd, arity: Int): List<Int> {
        val modes = cmd.modes(arity)
        return (0 until arity).map { getParamAddress(pos + it + 1, modes[it]) }
    }

    private fun getParamAddress(pos: Int, mode: Mode): Int {
        return when (mode) {
            Mode.POSITION -> code[pos]?.toInt() ?: pos
            Mode.IMMEDIATE -> pos
            Mode.RELATIVE -> relativeBase + (code[pos]?.toInt() ?: 0)
        }
    }

    fun sendString(s: String) {
        s.forEach { input.addLast(it.code.toLong()) }
        input.addLast('\n'.code.toLong())
    }

    fun getOutput(): Long? {
        return if (output.isNotEmpty()) output.removeFirst() else null
    }
}

enum class Mode {
    POSITION,
    IMMEDIATE,
    RELATIVE
}

class Cmd(private val instruction: Long) {
    fun opCode(): Int = (instruction % 100).toInt()

    fun modes(arity: Int): List<Mode> {
        val modeSection = instruction / 100
        return (0 until arity).map {
            val mode = (modeSection / 10.0.pow(it).toLong() % 10).toInt()
            when (mode) {
                0 -> Mode.POSITION
                1 -> Mode.IMMEDIATE
                2 -> Mode.RELATIVE
                else -> throw IllegalArgumentException("Invalid mode: $mode")
            }
        }
    }
}

fun main() {
    val vm = VM("input.txt")
    val instructions = listOf(
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "WALK"
    )

    instructions.forEach { vm.sendString(it) }

    vm.run()

    while (true) {
        val output = vm.getOutput() ?: break
        if (output > 127) {
            println(output)
            return
        }
    }
}
